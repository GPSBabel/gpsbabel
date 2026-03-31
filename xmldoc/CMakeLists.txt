if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
  message(FATAL_ERROR "Please use CMakeLists.txt in the project root directory to generate a build system.")
endif()

find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
  # The path to the xmldoc source directory needs to be a valid uri for xmliint.
  # Paths that contain spaces or many other non alphanumeric characters need to be uri escaped.
  execute_process(
    COMMAND ${Python3_EXECUTABLE} -c "import sys,urllib.parse; print(urllib.parse.quote(sys.argv[1]))" "${CMAKE_SOURCE_DIR}/xmldoc"
    OUTPUT_VARIABLE doc_src_path OUTPUT_STRIP_TRAILING_WHITESPACE)
  configure_file(readme.xml.in readme.xml @ONLY NEWLINE_STYLE LF)

  set(GPSBABEL_WEB "gpsbabel.org" CACHE PATH "Path where the documentation will be stored for www.gpsbabel.org.")

  find_program(JING NAMES jing)
  # jing and many depedencies removed from fedora
  if(JING STREQUAL "JING-NOTFOUND")
    message(STATUS "jing not found, skipping jing validation of xml document.")
  else()
    set(jing_rng_cmd ${JING} ${CMAKE_SOURCE_DIR}/tools/schema/docbook/xml/5.0/rng/docbook.rng readme.xml)
    set(jing_sch_cmd ${JING} ${CMAKE_SOURCE_DIR}/tools/schema/docbook/xml/5.0/sch/docbook.sch readme.xml)
  endif()

  add_custom_target(xmldoc
                    COMMAND perl ${CMAKE_CURRENT_SOURCE_DIR}/makedoc --pname $<TARGET_FILE:gpsbabel>
                    COMMAND xmllint --noout --relaxng file://${CMAKE_SOURCE_DIR}/tools/schema/docbook/xml/5.0/rng/docbook.rng readme.xml
                    # schematron validation with xmllint doesn't seem to work:
                    # "Schematron schema .../docbook.sch failed to compile"
                    # COMMAND xmllint --noout --schematron file://${CMAKE_SOURCE_DIR}/tools/schema/docbook/xml/5.0/sch/docbook.sch readme.xml
                    COMMAND ${jing_rng_cmd}
                    COMMAND ${jing_sch_cmd}
                    DEPENDS gpsbabel
                    VERBATIM
                    USES_TERMINAL)

  add_custom_target(gpsbabel.org
                    # the img.src.path must allow the images to be found on the web server.
                    COMMAND xsltproc
                            --stringparam base.dir ${GPSBABEL_WEB}/htmldoc-${GPSBABEL_DOCVERSION}/
                            --stringparam root.filename index
                            --stringparam img.src.path images/
                            ${CMAKE_CURRENT_SOURCE_DIR}/babelmain.xsl
                            readme.xml
                    COMMAND ${CMAKE_SOURCE_DIR}/tools/fixdoc ${GPSBABEL_WEB}/htmldoc-${GPSBABEL_DOCVERSION} "GPSBabel ${GPSBABEL_DOCVERSION}:"
                    COMMAND ${CMAKE_SOURCE_DIR}/tools/mkcapabilities ${GPSBABEL_WEB} ${GPSBABEL_WEB}/htmldoc-${GPSBABEL_DOCVERSION} $<TARGET_FILE:gpsbabel>
                    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/images ${GPSBABEL_WEB}/htmldoc-${GPSBABEL_DOCVERSION}/images
                    COMMAND ${CMAKE_COMMAND} -E copy gpsbabel.pdf ${GPSBABEL_WEB}/htmldoc-${GPSBABEL_DOCVERSION}/gpsbabel-${GPSBABEL_DOCVERSION}.pdf
                    DEPENDS xmldoc gpsbabel.pdf
                    VERBATIM
                    USES_TERMINAL)

  add_custom_target(gpsbabel.html
                    # the img.src.path must allow the images to be found on the web server.
                    COMMAND xsltproc
                            --output gpsbabel.html/index.html
                            --stringparam toc.section.depth 1
                            --stringparam html.cleanup 1
                            --stringparam make.clean.html 1
                            --stringparam html.valid.html 1
                            --stringparam html.stylesheet gpsbabel-doc.css
                            --stringparam img.src.path images/
                            http://docbook.sourceforge.net/release/xsl-ns/current/xhtml/docbook.xsl
                            readme.xml
                    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/images gpsbabel.html/images 
                    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gpsbabel-doc.css gpsbabel.html/gpsbabel-doc.css
                    # workaround invalid docbook.css https://github.com/docbook/xslt10-stylesheets/issues/276
                    COMMAND ${Python3_EXECUTABLE} -c [=[import re,sys; path=sys.argv[1]; s=open(path,'r',encoding='utf-8').read(); s=re.sub(r'hr.footnote-hr {\n  width: 100;\n}','hr.footnote-hr {\n  width: 100%;\n}',s); open(path,'w',encoding='utf-8').write(s);]=] gpsbabel.html/docbook.css
                    DEPENDS xmldoc
                    VERBATIM
                    USES_TERMINAL)

  add_custom_target(gpsbabel.pdf
                    # the img.src.path must allow the document images to be found when creating the pdf.
                    # gpsbabel.icon.path must allow the appicon to be found when creating the pdf.
                    COMMAND xsltproc 
                            -o gpsbabel.fo
                            --stringparam img.src.path ${CMAKE_CURRENT_SOURCE_DIR}/images/
                            --stringparam gpsbabel.icon.path ${CMAKE_SOURCE_DIR}/gui/images
                            ${CMAKE_CURRENT_SOURCE_DIR}/babelpdf.xsl readme.xml
                    COMMAND ${CMAKE_COMMAND} -E env HOME=${CMAKE_SOURCE_DIR} fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
                    DEPENDS xmldoc
                    VERBATIM
                    USES_TERMINAL)
else()
  message(WARNING "Document generation requires python3 (and perl5, xmllint, xsltproc, fop)")
endif()
