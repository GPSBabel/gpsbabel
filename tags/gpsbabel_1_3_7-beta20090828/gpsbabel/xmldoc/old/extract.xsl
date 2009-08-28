<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="id" />
<xsl:output method="xml" omit-xml-declaration="yes" />
<xsl:template match="/"><xsl:apply-templates select="//section[@id=$id]" mode="pr" /></xsl:template>
<xsl:template match="sectioninfo" mode="pr"></xsl:template>
<xsl:template match="title" mode="pr"></xsl:template>
<xsl:template match="section" mode="pr"><xsl:apply-templates mode="pr" /></xsl:template>
<xsl:template match="*" mode="pr" priority="-1"><xsl:copy-of select='.' /></xsl:template>
</xsl:stylesheet>
