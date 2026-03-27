<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:exsl="http://exslt.org/common"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns:xlink="http://www.w3.org/1999/xlink"
		version="1.0"
                exclude-result-prefixes="exsl">


<xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>

<!-- turn on extensions for newer versions of fop.  In particular, this makes
     the XSL generate an fo bookmark-tree, which fop translates into bookmarks
     in the PDF.   RLP -->
<xsl:param name="fop1.extensions" select="1" />
<xsl:param name="title.font.family">sans-serif</xsl:param>
<xsl:param name="body.font.family">serif</xsl:param>
<xsl:param name="sans.font.family">sans-serif</xsl:param>
<xsl:param name="monospace.font.family">monospace</xsl:param>
<xsl:param name="symbol.font.family">sans-serif</xsl:param>
<xsl:param name="dingbat.font.family">sans-serif</xsl:param>
<xsl:param name="gpsbabel.icon.path" select="gui/images"/>

<!-- Branding Colors -->
<xsl:variable name="gpsbabel.blue">#0054a6</xsl:variable>

<!-- Title Page Customization -->
<xsl:template name="book.titlepage.before.recto">
  <fo:block text-align="center" space-after="2in" space-before="1in">
    <fo:external-graphic src="url({$gpsbabel.icon.path}/appicon.png)" content-width="3in" content-height="3in" scaling="uniform"/>
  </fo:block>
</xsl:template>

<xsl:attribute-set name="book.titlepage.recto.style">
  <xsl:attribute name="text-align">center</xsl:attribute>
</xsl:attribute-set>

<!-- Title Styling -->
<xsl:attribute-set name="section.title.properties">
  <xsl:attribute name="color"><xsl:value-of select="$gpsbabel.blue"/></xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="chapter.title.properties">
  <xsl:attribute name="color"><xsl:value-of select="$gpsbabel.blue"/></xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="component.title.properties">
  <xsl:attribute name="color"><xsl:value-of select="$gpsbabel.blue"/></xsl:attribute>
</xsl:attribute-set>

<!-- Link and Xref Styling -->
<xsl:attribute-set name="xref.properties">
  <xsl:attribute name="color"><xsl:value-of select="$gpsbabel.blue"/></xsl:attribute>
  <xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<!-- External Link Styling -->
<xsl:template match="db:uri|db:link[@xlink:href]">
  <fo:inline color="{$gpsbabel.blue}" font-weight="bold">
    <xsl:apply-imports/>
  </fo:inline>
</xsl:template>

<!-- Table Styling -->
<xsl:template name="table.cell.properties">
  <xsl:if test="ancestor::db:thead">
    <xsl:attribute name="background-color">#f0f0f0</xsl:attribute>
  </xsl:if>
</xsl:template>

<!-- Admonition Styling (Notes, Warnings, etc) -->
<xsl:attribute-set name="admonition.properties">
    <xsl:attribute name="border">1pt solid #cccccc</xsl:attribute>
    <xsl:attribute name="background-color">#f9f9f9</xsl:attribute>
    <xsl:attribute name="padding">6pt</xsl:attribute>
    <xsl:attribute name="margin-left">0pt</xsl:attribute>
    <xsl:attribute name="margin-right">0pt</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="admonition.title.properties">
    <xsl:attribute name="color"><xsl:value-of select="$gpsbabel.blue"/></xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
</xsl:attribute-set>

<!-- This template formats userinput as a block-level element and adds the
     background and border we use in the HTML doc, for consistency.  RLP -->
<xsl:template match="db:userinput">
  <fo:block background-color="#E5E9EB" padding="4pt"
		break-after="auto" border="1.5pt solid #cccccc">
    <xsl:call-template name="inline.boldmonoseq"/>
  </fo:block>
</xsl:template>

<!-- Wrap any long lines in verbatim elements, which presumably use monospace.
     This is preferrable to truncating the lines, but manually breaking the line
     allows for the appropriate level of indent and control over where the break
     is. -->
<xsl:attribute-set name="monospace.verbatim.properties">
    <xsl:attribute name="wrap-option">wrap</xsl:attribute>
</xsl:attribute-set>

<!-- This template is used to get rid of a lot of warnings we were getting
     from fop due to the fact that it doesn't support table-layout="auto".
     Auto is apparently the default if no table layout is specified. RLP -->
<xsl:template match="db:simplelist">
  <!-- with no type specified, the default is 'vert' -->
  <xsl:variable name="explicit.table.width">
    <xsl:call-template name="dbfo-attribute">
      <xsl:with-param name="pis"
                      select="processing-instruction('dbfo')"/>
      <xsl:with-param name="attribute" select="'list-width'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="table.width">
    <xsl:choose>
      <xsl:when test="$explicit.table.width != ''">
        <xsl:value-of select="$explicit.table.width"/>
      </xsl:when>
      <xsl:when test="$default.table.width = ''">
        <xsl:text>100%</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$default.table.width"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <fo:table xsl:use-attribute-sets="normal.para.spacing">
    <xsl:attribute name="table-layout">fixed</xsl:attribute>
    <xsl:attribute name="width">
      <xsl:value-of select="$table.width"/>
    </xsl:attribute>
    <xsl:call-template name="simplelist.table.columns">
      <xsl:with-param name="cols">
        <xsl:choose>
          <xsl:when test="@columns">
            <xsl:value-of select="@columns"/>
          </xsl:when>
          <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
    <fo:table-body start-indent="0pt" end-indent="0pt">
      <xsl:call-template name="simplelist.vert">
        <xsl:with-param name="cols">
          <xsl:choose>
            <xsl:when test="@columns">
              <xsl:value-of select="@columns"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:call-template>
    </fo:table-body>
  </fo:table>
</xsl:template>


</xsl:stylesheet>
