#include <QtCore/QString>
#include <QtCore/QRegexp>

#include "shortname.h"

const QString kVowels("aeiou");

static struct replacements {
  const char *orig;
  const char *replacement;
} replacements[] = {
  {"zero",        "0"},
  {"one",         "1"},
  {"two",         "2"},
  {"three",       "3"},
  {"four",        "4"},
  {"five",        "5"},
  {"six",         "6"},
  {"seven",       "7"},
  {"eight",       "8"},
  {"nine",        "9"},
  {NULL,          NULL}
};


QString Shortname::DeleteLastVowel(int start, const QString& s,
                                   bool& replaced) {
 QString out = s;
 replaced = false;
#if 1
  for (int i = s.length() -1; i > start; i--) {
    QChar c = s[i];
    // Preserve vowel if first of a word.
    if (i > 0)  {
      if (s[i-1].isSpace())
        continue;
    }
    if (kVowels.contains(c, Qt::CaseInsensitive)) {
      out = out.remove(i, 1);
      replaced = true;
    }
  }
#endif
  return out;
}

QString Shortname::Replace(const QString& in) {
  QString out = in;

  struct replacements *r;
  for (r = replacements; r->orig; r++) {
//    while(int i = out.indexOf(r->orig, 1) > 2) {
   int i = out.indexOf(r->orig, 1);
   if(i > 1) {
fprintf(stderr, "%d\n", i);
      if (out[i-1]  == ' ') {
        out = out.replace(i, strlen(r->orig), r->replacement);
      }
    }
  }
  return out;
}

QString Shortname::Shorten(const QString& in) {
  // Eliminate leading and trailing whitespace and replace internal
  // whitespace sequences (tabs, multiple spaces, etc.) with a single space.
  QString out = in.simplified();

  if ((target_len_ == 6) && (out.length() == 7) && out.startsWith("GC")) {
    out = out.mid(1);
  }

  // Whack leading 'the'.
  if (out.startsWith("tHe ", Qt::CaseInsensitive)) {
    out = out.mid(4);
  }

  if (!whitespace_ok_) {
    out = out.replace(' ', "");
  }

  if (must_upper_) {
    out = out.toUpper();
  }

  out = Replace(out);

  // Strip forbidden characters.
  out.remove(QRegExp("[" + bad_chars_ + "]"));

  bool replaced = true;
  if (target_len_ > 14) {
    replaced = false;
  }
  while (replaced && out.length() > target_len_) {
    out = DeleteLastVowel(1, out, replaced);
  }

  // Preserve any trailing digits.
  int trailing_digits = out.indexOf(QRegExp("[0-9]*$"));
  if ((out.length() > target_len_) && (trailing_digits > 0)) {
    int digit_len = out.length() - trailing_digits;
    out.remove(target_len_ - digit_len, out.length() - target_len_);
  } else {
    out.truncate(target_len_);
  }

  if (must_unique_) {
    QString t(out);
    while (name_list_.contains(t)) {
      printf("Conflict %d\n", conflict_counter_);
      QString trailer = QString(".%1").arg(conflict_counter_);
fprintf(stderr, "Trailer: %s\n", qPrintable(trailer));
      t.truncate(target_len_ - trailer.length());
fprintf(stderr, "Truncate: %s\n", qPrintable(t));
      t = t.append(trailer);
      conflict_counter_++;
    }
    out = t;
    name_list_.insert(out);
  }
 fprintf(stderr, "Returning %s\n", qPrintable(out));
  assert(out.length() <= target_len_);
  return out;
}

#if 1
char *foo[] =  {
"VwthPst# 3700.706N 08627.588W 0000000m View the Past #2              ",
"PilotRoc 3655.270N 08717.173W 0000000m Pilot Rock                    ",
"MrCycsNg 3652.407N 08728.890W 0000000m Mr. Cayces Neighborhood      ",
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
"BltzMcr4 3517.127N 08622.211W 0000000m Blitz Micro Number 4          ",
"NkdnthWn 3437.145N 08651.693W 0000000m Naked in the Wind by Zaybex   ",
"ANcPlctR 3603.389N 08654.418W 0000000m A Nice Place to Rest by JoGPS ",
"welcomtT 3638.155N 08720.130W 0000000m welcome to TN by Raf of the se",
"welcomtK 3638.956N 08721.011W 0000000m welcome to KY by raf of the se",
"BltzMcr5 3506.191N 08634.277W 0000000m Blitz Micro Number 5          ",
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
"FrgttnP2 3509.599N 08633.282W 0000000m Forgotten Park 2              ",
"SOLDIERS 3640.691N 08726.660W 0000000m SOLDIERS TRIBUTE by Feros Fami",
"EndofRop 3433.820N 08650.460W 0000000m End of Rope by Big Rock       ",
"VwthPst1 3659.263N 08627.114W 0000000m View the Past #1              ",
"VwthPst2 3700.706N 08627.588W 0000000m View the Past #2              ",
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
"FrgttnPr 3509.599N 08633.282W 0000000m Forgotten Park                ",
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
"TrshOt15 3558.964N 08646.064W 0000000m Cache In Trash Out  # 15      ",
"TrshOt13 3602.214N 08650.428W 0000000m Cache In Trash Out #13        ",
"PrcysDrp 3604.312N 08653.465W 0000000m Percys Dripping Springs by KLi",
"TrshOt14 3605.292N 08648.560W 0000000m Cache In Trash Out # 14       ",
"PrtrnGr5 3557.621N 08640.278W 0000000m Praetorian Guards Hail Caesar #5!",
"PrtrnGr4 3557.370N 08640.201W 0000000m Praetorian Guards Hail Caesar #4!",
"PrtrnGr3 3557.250N 08640.047W 0000000m Praetorian Guards Hail Caesar #3!",
"GrnMntnC 3439.120N 08631.100W 0000000m Green Mountain Cache by Steve ",
"TrshOt12 3605.330N 08635.817W 0000000m Cache In Trash Out #12        ",
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

    extern "C" {
      struct short_handle *mkshort_new_handle(void);
      char * mkshort (struct short_handle *, char*);
      void setshort_whitespace_ok(struct short_handle *, int n);
    }
main()
{
	char **foop = foo;
	bool r;
        Shortname sn;
//        printf("%s\n", qPrintable(sn.Shorten("The Troll")));
#if 0
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
#endif
#if 1
        struct short_handle *mh = mkshort_new_handle();
        setshort_whitespace_ok(mh, 0);

        sn.target_length(8);
        sn.whitespace_ok(false);
        
	while (*foop) {
//		printf("%s %s\n", mkshort(*foop+39), *foop+39);
		QString s = sn.Shorten(*foop + 39);
                QString r = (*foop);
                r.truncate(8);
//fprintf(stderr, "zzz%s\n", *foop + 39);
                char *os = mkshort (mh, *foop + 39);
if (s != os) {
    fprintf(stderr, "%s/%s\n", qPrintable(r), os);
}
                // assert(r == s);
		foop++;
	}
#endif
#define X(a,b)  assert(a == sn.Shorten(b));
  X("GC12345", "GC12345");
  sn.target_length(6);
  X("C12345", "GC12345");
  X("abc", " abc ");
  X("a bc", " a\t\tbc ");
  X("a b c", " a b c ");
  sn.whitespace_ok(false);
  X("def", " d e f  ");
  sn.must_upper(true);
  X("GHI", " g h i  ");

  sn.whitespace_ok(true);
  sn.must_upper(false);
//  X("pickle 2", "pickle two");
//  X("two 2", "two two");
//  X("one 1 throne", " one one throne ");

  X("dori", "do-ri");;
  X("dora", "do$ra");
  sn.badchars("or");
  X("d-i", "do-ri");
  X("d$i", "do$ri");
  sn.badchars(QString());

  sn.target_length(6);
  X("10", "10");
  X("a10", "a10");
  X("aa10", "aa10");
  X("aaa10", "aaa10");
  X("baaa10", "baaa10");
  X("ccccc0", "ccccccc0");
  X("dccc10", "dcccccccccccccccc10");

  X("zark", "zark");
  X("zark.0", "zark");
  X("zark.1", "zark");
  X("zark.2", "zark");
  X("zark.3", "zark");
  X("zark.4", "zark");

  sn.target_length(8);
  X("quick br", "the quick brown fox");
  X("quick .5", "the quick brown foo");
  X("aexx", "aexx");
  X("aeab1234", "aeabcdefg1234");
  X("eat a pc", "eat a peach");
  X("eat a or", "eat a orange");

 printf("%s\n", qPrintable(Shortname::DeleteLastVowel(5, "eat a peach", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(5, "eat a orange", r)));


printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "the quick brown foo", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "the quick brown fox", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "eat a peach", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(5, "eat a peach", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(5, "eat a orange", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "xxx", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "ixxx", r)));
printf("%s\n", qPrintable(Shortname::DeleteLastVowel(0, "aexxx", r)));
	
}
#endif
