cmake_minimum_required(VERSION 3.5)
project(swipl-ltx2htm)

include("../cmake/PrologPackage.cmake")

set(PL_LIB latex2html.pl latex.cmd sty_pldoc.pl pldoc.cmd sty_xpce.pl xpce.cmd
    latex2html.css)
set(ICONS home.gif index.gif info.gif next.gif prev.gif up.gif
    yellow_pages.gif)
prepend(ICONS icons/ ${ICONS})

set(SWIPL ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH}/swipl)
configure_file(latex2html.in latex2html)

# TODO: The following doesn't work:
# pkg_doc(
#     ltx2htm
#     SOURCE latex2html.pl ltx2htm.doc)

swipl_plugin(
    latex2html
    NOINDEX
    MODULE tex
    C_SOURCES tex.c psfile.c
    PL_LIBS ${PL_LIB}
    PL_LIB_SUBDIR icons
        PL_LIBS ${ICONS})
# Needed to run latex2html
add_dependencies(latex2html clib)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/latex2html
	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin)
install_src(latex2html_app
	    FILES app.pl
	    RENAME latex2html.pl
	    DESTINATION ${SWIPL_INSTALL_APP})
