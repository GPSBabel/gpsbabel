<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:exsl="http://exslt.org/common"
		version="1.0"
                exclude-result-prefixes="exsl">


<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>

<!-- turn on extensions for newer versions of fop.  In particular, this makes
     the XSL generate an fo bookmark-tree, which fop translates into bookmarks
     in the PDF.   RLP -->
<xsl:param name="fop1.extensions" select="1" />

<!-- This template formats userinput as a block-level element and adds the
     background and border we use in the HTML doc, for consistency.  RLP -->
<xsl:template match="userinput">
  <fo:block background-color="#E5E9EB" padding="6pt" 
		border="1pt dashed #000000">
    <xsl:call-template name="inline.boldmonoseq"/>
  </fo:block>
</xsl:template>

<!-- This template is used to get rid of a lot of warnings we were getting
     from fop due to the fact that it doesn't support table-layout="auto".
     Auto is apparently the default if no table layout is specified. RLP -->
<xsl:template match="simplelist">
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
