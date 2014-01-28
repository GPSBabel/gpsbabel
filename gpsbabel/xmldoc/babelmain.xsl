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
</xsl:template>

<xsl:template name="user.footer.navigation">
  <xsl:text>
  </xsl:text>
</xsl:template>


<xsl:template name="chunk-element-content" priority="100">
  <xsl:param name="prev"/>
  <xsl:param name="next"/>
  <xsl:param name="nav.context"/>
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>

  {extends file="main.tpl"}
    <xsl:call-template name="html.head">
      <xsl:with-param name="prev" select="$prev"/>
      <xsl:with-param name="next" select="$next"/>
    </xsl:call-template>
      <xsl:call-template name="body.attributes"/>
  {block name=body}
                      <xsl:call-template name="user.header.navigation"/>
               
                      <xsl:call-template name="header.navigation">
                        <xsl:with-param name="prev" select="$prev"/>
                        <xsl:with-param name="next" select="$next"/>
                        <xsl:with-param name="nav.context" select="$nav.context"/>
                      </xsl:call-template>
               
                      <xsl:call-template name="user.header.content"/>
               
                      <xsl:copy-of select="$content"/>
               
                      <xsl:call-template name="user.footer.content"/>
               
                      <xsl:call-template name="footer.navigation">
                        <xsl:with-param name="prev" select="$prev"/>
                        <xsl:with-param name="next" select="$next"/>
                        <xsl:with-param name="nav.context" select="$nav.context"/>
                      </xsl:call-template>
               
                      <xsl:call-template name="user.footer.navigation"/>
      {/block}
  <xsl:value-of select="$chunk.append"/>
</xsl:template>

 
</xsl:stylesheet>
