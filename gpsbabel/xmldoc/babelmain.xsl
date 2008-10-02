<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
		version="1.0"
                exclude-result-prefixes="exsl">


<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/chunk.xsl"/>

<xsl:param name="html.stylesheet">http://www.gpsbabel.org/style3.css</xsl:param>
<xsl:param name="use.id.as.filename">1</xsl:param>
<xsl:param name="chunk.first.sections">1</xsl:param>
<xsl:param name="toc.section.depth">1</xsl:param>

<xsl:template name="user.header.navigation">
  <xsl:text>

  </xsl:text>
  <xsl:comment>#include virtual="../navbar.inc" </xsl:comment>
  <xsl:comment>#include virtual="../doc-header.inc" </xsl:comment>
  <xsl:text>

  </xsl:text>
</xsl:template>

<xsl:template name="user.footer.navigation">
  <xsl:text>

  </xsl:text>
  <xsl:comment>#include virtual="../doc-footer.inc" </xsl:comment>
  <xsl:text>

  </xsl:text>
</xsl:template>
 
</xsl:stylesheet>
