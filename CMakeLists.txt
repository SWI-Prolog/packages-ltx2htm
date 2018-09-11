cmake_minimum_required(VERSION 2.8.12)
project(swipl-ltx2htm)

include("../cmake/PrologPackage.cmake")

set(PL_LIB latex2html.pl latex.cmd pl.pl pl.cmd xpce.pl xpce.cmd
    latex2html.css)
set(ICONS home.gif index.gif info.gif next.gif prev.gif up.gif
    yellow_pages.gif)
prepend(ICONS icons/ ${ICONS})

add_compile_options(-D__SWI_PROLOG__)

swipl_plugin(
    tex
    C_SOURCES tex.c psfile.c
    PL_LIB_SUBDIR latex2html
    PL_LIBS ${PL_LIB})

install(FILES pl.pl
	DESTINATION ${SWIPL_INSTALL_LIBRARY}/latex2html
	RENAME pldoc.pl)
install(FILES pl.cmd
	DESTINATION ${SWIPL_INSTALL_LIBRARY}/latex2html
	RENAME pldoc.cmd)

swipl_plugin(
    latex2html_icons
    PL_LIB_SUBDIR latex2html/icons
    PL_LIBS ${ICONS})

install(PROGRAMS latex2html
	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin)
