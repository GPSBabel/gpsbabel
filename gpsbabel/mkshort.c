#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "defs.h"

static const char vowels[] = "aeiouAEIOU";

#define DEFAULT_TARGET_LEN 8
#define DEFAULT_BADCHARS "\"$.,'!-"

/*
 * Hash table tunings.   The reality is that our hash doesn't have to be 
 * terribly complex; our strings are short (typically 8-20 bytes) and the
 * string hash mixes things up enough that strcmp can generally bail on the
 * first byte or two for a mismatch.  
 */
#define PRIME 37

typedef struct {
	int mustupper;
	int whitespaceok;
	unsigned int target_len;
	char *badchars;
	int must_uniq;
	queue namelist[PRIME];
	int depth[PRIME];
} mkshort_handle;

typedef struct {
	queue list;
	char *orig_shortname;
	int conflictctr;
} uniq_shortname;

/* 
 * We hash all strings as upper case.
 */
unsigned int hash_string(const char *key)
{
	unsigned int hash = 0;
	while (*key) {
		hash = ((hash<<5) ^ (hash>>27)) ^ toupper(*key++);
	}
	hash = hash % PRIME;
	return hash;
}

void *
mkshort_new_handle()
{
	int i;
	mkshort_handle *h = xcalloc(sizeof *h, 1);

	for (i = 0; i < PRIME; i++)
		QUEUE_INIT(&h->namelist[i]);

	h->whitespaceok = 1;
	h->badchars = DEFAULT_BADCHARS;
	h->target_len = DEFAULT_TARGET_LEN;
	h->must_uniq=1;

	return h;
}

char *
mkshort_add_to_list(mkshort_handle *h, char *name)
{
	queue *e, *t;
	int hash;
	uniq_shortname *s = xcalloc(1, sizeof (uniq_shortname));
	s->orig_shortname = strdup(name);
	hash = hash_string(name);

	QUEUE_FOR_EACH(&h->namelist[hash], e, t) {
		uniq_shortname *z = (uniq_shortname *) e;

		if (0 == case_ignore_strcmp(z->orig_shortname, name)) {
			int l = strlen(name);
			int dl;
			char tbuf[10];

			z->conflictctr++;
			dl = sprintf(tbuf, ".%d", z->conflictctr);

			if (l + dl < h->target_len) {
				name = xrealloc(name, l + dl + 1);
				strcat(name, tbuf);
			}
			else {
				strcpy(&name[l-dl], tbuf);
			}
			break;
		}
	}
	ENQUEUE_TAIL(&h->namelist[hash], &s->list);
	h->depth[hash]++;
	return name;
}

void *
mkshort_del_handle(void *h)
{
	mkshort_handle *hdr = h;
	int i;

	if (hdr) {
		for (i = 0; i < PRIME; i++) {
			queue *e, *t, *z;
			QUEUE_FOR_EACH(&hdr->namelist[i], e, t) {
				uniq_shortname *s = (uniq_shortname *) e;
				dequeue(e);
				free(s->orig_shortname);
				free(s);
			}
		}
		free(hdr);
	}
}

/*
 * This is the stuff that makes me ashamed to be a C programmer...
 */

static 
char *
delete_last_vowel(int start, char *istring, int *replaced)
{
	int l;
	
	/*
	 * Basically impelement strrchr.
	 */
	for (l = strlen(istring); l > start; l--) {
		if (strchr(vowels, istring[l-1])) {
			char *ostring = xstrdup(istring);

			strncpy(&ostring[l-1], &istring[l], 1+strlen(istring)-l);
			ostring[strlen(istring)-1] = 0;
			*replaced = 1;
			return ostring;
		}
	}
	*replaced = 0;
	return istring;

}

/*
 * Externally callable function to set the max length of the
 * strings returned by mkshort().  0 resets to default.
 */
void
setshort_length(void *h, int l)
{
	mkshort_handle *hdl = h;
	if (l == 0) {
		hdl->target_len = DEFAULT_TARGET_LEN;
	} else {
		hdl->target_len = l;
	}
}

void
setshort_whitespace_ok(void *h, int l)
{
	mkshort_handle *hdl = h;
	hdl->whitespaceok = l;
}

/*
 * Externally callable function to set the string of characters
 * that must never appear in a string returned by mkshort.  NULL
 * resets to default.
 */
void
setshort_badchars(void *h, const char *s)
{
	mkshort_handle *hdl = h;
	if (s == NULL) {
		hdl->badchars = DEFAULT_BADCHARS;
	} else {
		hdl->badchars = xstrdup(s);
	}
}

void
setshort_mustupper(void *h, int i)
{
	mkshort_handle *hdl = h;
	hdl->mustupper = i;
}

void
setshort_mustuniq(void *h, int i)
{
	mkshort_handle *hdl = h;
	hdl->must_uniq = i;
}


char *
mkshort(void *h, const char *istring)
{
	char *ostring = xstrdup(istring);
	char *nstring;
	char *tstring;
	char *cp;
	char *np;
	int i, l, nlen, replaced;
	mkshort_handle *hdl = h;

	/* 
	 * Whack leading "[Tt]he",
 	 */
	if (( strlen(ostring) > hdl->target_len + 4) && 
	    (strncmp(ostring, "The ", 4) == 0 || 
	    strncmp(ostring, "the ", 4) == 0)) {
		nstring = xstrdup(ostring + 4);
		free(ostring);
		ostring = nstring;
	}

	/*
	 * Look at the back of the string for " by BLAH" and whack 
	 * it there.
	 */
	nstring = xstrdup(ostring);
	l = strlen (nstring);
	while (l > 0) {
		if (strncmp(&nstring[l], " by ",4) == 0)  {
			nstring[l] = 0;
			break;
		}
		l --;
	}
	free(ostring);
	ostring = nstring;

	if (!hdl->whitespaceok) {
		/* 
		 * Eliminate Whitespace 
		 */
		tstring = xstrdup(ostring);
		l = strlen (tstring);
		cp = ostring;
		for (i=0;i<l;i++) {
			if (!isspace(tstring[i])) {
				if (hdl->mustupper) {
					tstring[i] = toupper(tstring[i]);
				}
				*cp++ = tstring[i];
			}
		}
		free(tstring);
		*cp = 0;
	}

	/*
	 * Eliminate chars on the blacklist.
	 * Characters that aren't ASCII are never OK.
 	 */
	tstring = xstrdup(ostring);
	l = strlen (tstring);
	cp = ostring;
	for (i=0;i<l;i++) {
		if (strchr(hdl->badchars, tstring[i]) || !isascii(tstring[i]))
			continue;
		*cp++ = tstring[i];
	}
	*cp = 0;
	free(tstring);

	/*
	 * Toss vowels to approach target length, but don't toss them 	
	 * if we don't have to.  We always keep the leading two letters
	 * to preserve leading vowels and some semblance of pronouncability.
	 *
	 * FIXME: There's a boundary case here where we're too aggressive.
	 * If the target length is "6" we will shorten "Trolley" to "Trlly",
	 * when we really don't have to.  We should throw away one vowel, not
	 * both.
	 */

	tstring = xstrdup(ostring);

	/*
	 * Delete vowels starting from the end.  If it fits, quit stomping
	 * them.  If we run out of string, give up.
	 */
	replaced = 1;
	while (replaced && strlen(ostring) > hdl->target_len) {
		ostring = delete_last_vowel(2, ostring, &replaced);
	}
	
	/*
	 * Next to last thing, we look for trailing numbers and try to 
	 * preserve those.  This ensures that.
	 * Walk in the Woods 1.
	 * Walk in the Woods 2.
	 */
	np = ostring + strlen(ostring);
	while (isdigit(*(np-1) )) {
		np--;
	}
	if (np) {
		nlen = strlen(np);
	}

	/*
	 * Now brutally truncate the resulting string, preserve trailing 
	 * numeric data.
 	 */
	if ((/*i = */strlen(ostring)) > hdl->target_len) {
		strcpy(&ostring[hdl->target_len] - strlen(np), np);
	}


	if (hdl->must_uniq) {
		return mkshort_add_to_list(hdl, ostring);
	}

	return ostring;
}

#if 0
char *foo[] =  {
"VwthPst# 3700.706N 08627.588W 0000000m View the Past #2              ",
"PilotRoc 3655.270N 08717.173W 0000000m Pilot Rock by CacheAdvance    ",
"MrCycsNg 3652.407N 08728.890W 0000000m Mr. Cayces Neighborhood by Ca",
"SOLDIER  3640.691N 08726.660W 0000000m SOLDIER&#8217;S TRIBUTE       ",
"ZOOMZOOM 3636.659N 08721.793W 0000000m ZOOM ZOOM ZOOM by Feros Family",
"SOCLOSEB 3636.494N 08722.086W 0000000m SO CLOSE BUT YET by Kyle of Fe",
"InSrchfS 3636.363N 08636.363W 0000000m In Search of Steam by BigHank ",
"RdBlngSp 3632.119N 08550.956W 0000000m Red Boiling Springs by Firedog",
"HelngWtr 3631.729N 08550.481W 0000000m Healing Waters by FiredogPotte",
"AHHtheVi 3629.020N 08533.891W 0000000m ogPotter                      ",
"LstCrkCc 3628.167N 08801.656W 0000000m Lost Creek Cache by Paul Kathy",
"DlvrncTr 3626.412N 08729.249W 0000000m Deliverance Train by Team Skay",
"FrQrtrRn 3438.502N 08646.926W 0000000m Four Quarter Rendezvous by Zay",
"Jstlttlc 3620.647N 08814.298W 0000000m Just a little cache by Paul Ka",
"BrryPtch 3618.786N 08616.344W 0000000m Berry Patch Cache by White Dog",
"AStrllDw 3342.752N 08630.829W 0000000m A Stroll Down Memory Lane by t",
"QunfTnns 3606.413N 08651.962W 0000000m Queen of Tennessee by A182pilo",
"GoneFish 3618.199N 08655.171W 0000000m Gone Fishin' by White Dog Pack",
"GrnwysFn 3610.942N 08642.061W 0000000m Greenways Fence by Ukulele And",
"AStnsThr 3611.240N 08638.324W 0000000m A Stone's Throw by Murrcat & S",
"Nashvlls 3617.112N 08642.359W 0000000m Nashville's Zoo by White Dog P",
"BltzMcr4 3517.127N 08622.211W 0000000m Blitz Micro Number 4 by IHTFP ",
"NkdnthWn 3437.145N 08651.693W 0000000m Naked in the Wind by Zaybex   ",
"ANcPlctR 3603.389N 08654.418W 0000000m A Nice Place to Rest by JoGPS ",
"welcomtT 3638.155N 08720.130W 0000000m welcome to TN by Raf of the se",
"welcomtK 3638.956N 08721.011W 0000000m welcome to KY by raf of the se",
"BltzMcr5 3506.191N 08634.277W 0000000m Blitz Micro Number 5 by IHTFP ",
"JmsFmlyG 3615.887N 08649.846W 0000000m James Family Grocery by White ",
"seekngrf 3629.262N 08742.333W 0000000m seekeing refuge by raf of the ",
"SecrtFll 3614.927N 08534.180W 0000000m Secret Falls                  ",
"ApstlcTh 3613.870N 08645.108W 0000000m Apostolic Thistle Walk by Jame",
"WllIllBD 3609.258N 08637.268W 0000000m Well....I'll Be \"Dammed\" byi",
"BettysBt 3608.857N 08550.564W 0000000m Betty's Booty by White Dog Pac",
"SmthngSm 3439.748N 08643.522W 0000000m Something Smells Fishy by Zayb",
"RckyRd(C 3605.315N 08549.326W 0000000m Rocky Road (Center Hill Lake) ",
"Brdwtchr 3436.605N 08651.243W 0000000m Birdwatcher's Dream by Zaybex ",
"JcksnsHl 3605.185N 08619.439W 0000000m Jackson's Halls by White Dog P",
"FrgttnP2 3509.599N 08633.282W 0000000m Forgotten Park 2 by mdawg & mu",
"SOLDIERS 3640.691N 08726.660W 0000000m SOLDIERS TRIBUTE by Feros Fami",
"EndofRop 3433.820N 08650.460W 0000000m End of Rope by Big Rock       ",
"VwthPst1 3659.263N 08627.114W 0000000m View the Past #1 by wkgraham  ",
"VwthPst2 3700.706N 08627.588W 0000000m View the Past #2 by wkgraham  ",
"Trash#8  3603.102N 08655.144W 0000000m Cache In Trash Out # 8        ",
"SlwwwwCc 3602.543N 08535.953W 0000000m Sloowwww Cache by Tntcacher   ",
"Leavttbv 3602.514N 08638.686W 0000000m Leave it to beaver by A182pilo",
"WhrrthHr 3436.594N 08654.759W 0000000m Where are the Horses? by Zaybe",
"ButtonCc 3433.401N 08645.294W 0000000m Button Cache by Zaybex        ",
"MrcsLbrr 3436.842N 08655.972W 0000000m Marco's Library by Marco      ",
"Octopus  3526.743N 08534.757W 0000000m Octopus by White Dog Pack     ",
"WtrFllsV 3544.140N 08527.861W 0000000m Water Falls Valley by Cave Rat",
"DeddrpPn 3448.126N 08719.696W 0000000m Dead-drop Pink by Marco       ",
"JWhlrRvr 3448.157N 08719.914W 0000000m Joe Wheeler River Walk by Marc",
"CvSprngs 3432.797N 08651.084W 0000000m Cave Springs Cache by Marco.. ",
"CnyFrkOv 3550.876N 08518.446W 0000000m Fork Overlook                 ",
"SheepsCa 3550.527N 08519.480W 0000000m Sheep's Cave                  ",
"VrgnFlls 3550.308N 08519.904W 0000000m Virgin Falls Cache            ",
"ShrtctVr 3550.170N 08519.590W 0000000m Shortcut Virtual              ",
"KlylFlls 3549.105N 08539.814W 0000000m Klaylee Falls Cache by pattytr",
"FshngfrB 3548.923N 08538.558W 0000000m BADGER  by M&Mk               ",
"TpfthHll 3548.808N 08601.722W 0000000m Top of the Hill Pet Cache by M",
"TwnFllsC 3548.560N 08537.996W 0000000m  Twin Falls  Cache by SLCREW a",
"WtchsCst 3548.197N 08537.673W 0000000m Witch's Castle Keys by SLCREW ",
"ThatCave 3544.901N 08522.854W 0000000m That Cave by JaDan150 and AprJ",
"BssltwnW 3541.174N 08801.489W 0000000m Busseltown Wildlife Refuge by ",
"Riverfrn 3540.968N 08546.995W 0000000m  Riverfront  by SLCREW and M&M",
"Basement 3540.086N 08521.304W 0000000m The Basement                  ",
"EfflTwrC 3617.132N 08818.371W 0000000m Eiffel Tower Cache by Dick Wan",
"KeyholeC 3544.562N 08524.098W 0000000m Keyhole Cave by Cave Rat      ",
"(MC^2)Mn 3444.990N 08630.218W 0000000m (MC^2) Monte Sano Cuts Cache b",
"WildctCc 3636.823N 08808.087W 0000000m Wildcat Cache by The Storm    ",
"NAlbm/Tn 3444.365N 08632.688W 0000000m N. Alabama / Tennessee Valley ",
"CalebsCa 3444.215N 08633.103W 0000000m Caleb's Cave by Papaw and Cale",
"MntSnPrs 3444.201N 08632.591W 0000000m Monte Sano Preserve by Evan & ",
"FltRckFl 3444.475N 08629.958W 0000000m Flat Rock Falls Cache by Jason",
"PanormCc 3443.961N 08631.638W 0000000m The Panorama Cache by IHTFP an",
"TnnssScv 3602.861N 08652.751W 0000000m Tennessee Scavenger Hunt Cache",
"Geocache 3435.209N 08655.968W 0000000m Geocache by Papaw & Caleb     ",
"Skellig  3444.100N 08656.566W 0000000m Skellig by Zaybex             ",
"Deceptio 3433.450N 08655.711W 0000000m Deception by Papaw and Caleb  ",
"AwlknthD 3433.310N 08655.635W 0000000m A   walk in the Desert by Papa",
"MiniMsQs 3558.934N 08650.674W 0000000m Mini Me's Quest by CrotalusRex",
"BakrsBlf 3541.891N 08717.300W 0000000m Bakers Bluff by Flower Child &",
"GoFlyAKi 3435.664N 08659.267W 0000000m Go Fly A Kite by Marco..      ",
"FlntCrkA 3432.028N 08656.806W 0000000m Flint Creek Adventure by Marco",
"HonordMn 3534.680N 08612.557W 0000000m Honored Mount by Southpaw     ",
"SafariZo 3440.697N 08700.774W 0000000m Safari Zone by Zaybex         ",
"JckDnlsC 3517.077N 08622.260W 0000000m Jack Daniels Cache by Rmearse ",
"FrgttnPr 3509.599N 08633.282W 0000000m Forgotten Park by mdawg & muff",
"DntOvrlk 3513.326N 08616.031W 0000000m Dont Overlook Me Cache        ",
"ArYStmpd 3513.039N 08615.110W 0000000m Are You Stumped Yet? cache    ",
"CchtthBn 3512.532N 08614.691W 0000000m Cache at the Bend             ",
"Thtsnkng 3609.009N 08530.314W 0000000m That sinking feeling by Tntcac",
"GamersCc 3449.136N 08635.836W 0000000m mer's Cache by avoral         ",
"CchMIfYC 3452.455N 08620.648W 0000000m Cache Me If You Can! by Glen H",
"SavageVs 3526.915N 08535.136W 0000000m Savage Vista by White Dog Pack",
"PrtrnG15 3555.479N 08653.274W 0000000m Praetorian Guards Hail Caesar #15!",
"WtrlnAmp 3443.722N 08632.535W 0000000m Waterline Amphitheater by Fath",
"BysLttlC 3447.569N 08638.448W 0000000m Boys' Little Cache by Zaybex  ",
"DrgnsBrt 3443.779N 08635.188W 0000000m Dragon's Breath by Zaybex     ",
"CryBbyHl 3430.733N 08657.362W 0000000m Cry Baby Hollow Cache by La Pa",
"Parmer   3606.218N 08651.590W 0000000m Parmer by A182pilot & Family  ",
"JnnfrndJ 3438.141N 08632.991W 0000000m Jennifer and Jonathans Cliff C",
"ALDRIDGE 3435.305N 08632.868W 0000000m ALDRIDGE CREEK LOTTA LOOT!! by",
"RcktCtyS 3440.032N 08631.352W 0000000m Rocket City Stash by David Upt",
"TrgcAccd 3608.561N 08648.381W 0000000m Tragic Accident by Campaholics",
"FALLENTR 3439.210N 08631.104W 0000000m FALLEN TREE 4 MILE POST by zac",
"TrshOt15 3558.964N 08646.064W 0000000m Cache In Trash Out  # 15 by Jo",
"TrshOt13 3602.214N 08650.428W 0000000m Cache In Trash Out #13 by JoGP",
"PrcysDrp 3604.312N 08653.465W 0000000m Percys Dripping Springs by KLi",
"TrshOt14 3605.292N 08648.560W 0000000m Cache In Trash Out # 14 by JoG",
"PrtrnGr5 3557.621N 08640.278W 0000000m Praetorian Guards Hail Caesar #5!",
"PrtrnGr4 3557.370N 08640.201W 0000000m Praetorian Guards Hail Caesar #4!",
"PrtrnGr3 3557.250N 08640.047W 0000000m Praetorian Guards Hail Caesar #3!",
"GrnMntnC 3439.120N 08631.100W 0000000m Green Mountain Cache by Steve ",
"TrshOt12 3605.330N 08635.817W 0000000m Cache In Trash Out #12 by JoGP",
"BlncngAc 3608.579N 08648.120W 0000000m Balancing Act by Campaholics  ",
"DittoCac 3434.652N 08633.310W 0000000m Ditto Cache by mookey         ",
"EraserCc 3431.888N 08633.024W 0000000m Eraser Cache by Zaybex        ",
"FrMlPstE 3439.440N 08630.180W 0000000m Four Mile Post Extension Cache",
"MllrdFxC 3439.578N 08706.552W 0000000m Mallard-Fox Creek Cache by bam",
"FireCach 3443.908N 08630.318W 0000000m he by Glen Pam Chase M        ",
"FlntRvrC 3443.170N 08625.990W 0000000m Flint River Canoe Cache by Ran",
"ArabinCc 3419.104N 08628.765W 0000000m The Arabian Cache by WesNSpace",
"CvrdBrdg 3412.406N 08659.392W 0000000m Covered Bridge Cache by pmarkh",
"MilesTCc 3424.470N 08611.720W 0000000m Miles Too Cache by Rmearse    ",
"MbryOvrl 3423.803N 08611.922W 0000000m Mabrey Overlook Me by DDVaughn",
"LwEnfrcm 3423.218N 08612.258W 0000000m Law Enforcement Cache by DDVau",
"GrndDddy 3423.128N 08612.025W 0000000m Grand Daddys Home by Rmearse  ",
"BamaCach 3421.459N 08611.686W 0000000m The Bama Cache by DDVaughn & T",
"Canyons  3440.085N 08600.910W 0000000m The Canyons by Aubrey and Josh",
"ADamGodV 3511.677N 08616.587W 0000000m A Dam Good View by mdawg & muf",
"UNDERTHE 3446.918N 08739.790W 0000000m UNDER THE ROCK by RUNNINGWILD ",
"SQUIRREL 3448.712N 08741.681W 0000000m SQUIRREL'S NEST by RUNNINGWILD",
"WlknthPr 3448.273N 08741.844W 0000000m Walk in the Park by Runningwil",
"NetsClue 3448.494N 08741.977W 0000000m Net's Clues by Runningwild Adv",
"NatrlBrd 3405.583N 08736.909W 0000000m Natural Bridge by Southeast Xt",
"TrnglPrk 3341.448N 08640.980W 0000000m Triangle Park Cache by Charles",
"LttlRvrI 3421.855N 08539.597W 0000000m Little River Initiative by spa",
"GimmShlt 3430.087N 08536.834W 0000000m Gimme Shelter by Big Rock & Po",
"GnomeTrs 3433.081N 08535.849W 0000000m Gnome Treasure by Big Rock    ",
"FlyingTr 3608.594N 08648.179W 0000000m  Flying Torso  by Campaholics ",
"CultivtC 3608.582N 08648.064W 0000000m  Cultivate a Cure  by Campahol"
}
;

main()
{
	char **foop = foo;
	int r;

	printf("%s\n", mkshort("The Troll"));
	printf("%s\n", mkshort("EFI"));
	printf("%s\n", mkshort("the Troll"));
	printf("%s\n", mkshort("the Trolley"));
	printf("%s\n", mkshort("The Troll Lives Under The Bridge"));
	printf("%s\n", mkshort("The \"Troll\" Lives Under $$ The Bridge!"));
	printf("%s\n", mkshort("The Trolley Goes Round"));
	printf("%s\n", mkshort("Cache In / Trash Out #1"));
	printf("%s\n", mkshort("Cache In / Trash Out #2"));
	printf("%s\n", mkshort("Cache In / Trash Out #137"));
	
	while (0 && *foop) {
		printf("%s %s\n", mkshort(*foop+39), *foop+39);
		foop++;
	}

printf("%s\n", delete_last_vowel(0, "the quick brown foo", &r));
printf("%s\n", delete_last_vowel(0, "the quick brown fox", &r));
printf("%s\n", delete_last_vowel(0, "xxx", &r));
printf("%s\n", delete_last_vowel(0, "ixxx", &r));
printf("%s\n", delete_last_vowel(0, "aexxx", &r));
	
}
#endif
