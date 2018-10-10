cmake_minimum_required(VERSION 2.8.12)
project(swipl-ltx2htm)

include("../cmake/PrologPackage.cmake")

set(PL_LIB latex2html.pl latex.cmd sty_pldoc.pl pldoc.cmd sty_xpce.pl xpce.cmd
    latex2html.css)
set(ICONS home.gif index.gif info.gif next.gif prev.gif up.gif
    yellow_pages.gif)
prepend(ICONS icons/ ${ICONS})

set(SWIPL ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH}/swipl)
configure_file(latex2html.in latex2html)

swipl_plugin(
    latex2html
    MODULE tex
    C_SOURCES tex.c psfile.c
    PL_LIB_SUBDIR latex2html
        PL_LIBS ${PL_LIB}
    PL_LIB_SUBDIR latex2html/icons
        PL_LIBS ${ICONS})

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/latex2html
	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin)
