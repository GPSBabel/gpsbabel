<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
		version="1.0"
                exclude-result-prefixes="exsl">


<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/chunk.xsl"/>

<xsl:param name="use.id.as.filename">1</xsl:param>
<xsl:param name="chunk.first.sections">1</xsl:param>
<xsl:param name="toc.section.depth">1</xsl:param>

<xsl:template name="system.head.content">
  <xsl:text>
  </xsl:text>
  <xsl:comment> InstanceBegin template="/Templates/primarypage.dwt" codeOutsideHTMLisLocked="false" </xsl:comment>
  <xsl:text>
  </xsl:text>
</xsl:template>

<!--
<xsl:template name="system.footer.content">
  <xsl:text>
  </xsl:text>
  <xsl:comment> InstanceEnd </xsl:comment>
  <xsl:text>
  </xsl:text>
</xsl:template>
-->


<xsl:template name="user.header.navigation">
  <xsl:text> 
  </xsl:text>
  <xsl:comment> InstanceBeginEditable name="scontent" </xsl:comment> 
</xsl:template>

<xsl:template name="user.footer.navigation">
  <xsl:text>
  </xsl:text>
  <xsl:comment> InstanceEndEditable </xsl:comment>
  <xsl:text>
  </xsl:text>
  <xsl:comment> InstanceEnd </xsl:comment>
  <xsl:text>
  </xsl:text>
</xsl:template>
 
</xsl:stylesheet>
