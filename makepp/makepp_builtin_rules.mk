# $Id: makepp_builtin_rules.mk,v 1.27 2009/02/10 22:55:49 pfeiffer Exp $
# Please do NOT use this as an example of how to write a makefile.  This is
# NOT A typical makefile.
#
# These are the rules that are built into makepp, that it can apply without
# anything in the makefile.  Don't put too much junk in here!  One problem
# with GNU make that I ran into was that its default rules included stuff for
# dealing with very rare suffixes that I happened to use in a different
# and incompatible way.
#
# This file is read in after reading all of each makefile.
#
# It's not a good idea to put definitions of standard variables like CC, etc.,
# in here, because then (a) the $(origin ) function doesn't work; (b) the
# values won't be visible except in rules, because this file is loaded after
# everything else in the makefile is processed.	 Standard variables are
# implemented as functions that have no arguments (see Mpp/Subs.pm).
#

#
# Rules.  Special code in makepp makes it so these rules never override any
# kind of rule that is contained in a user makeppfile.
#
_CPP_SUFFIXES := cxx c++ cc cpp
_OBJ_SUFFIX = .o
_OUTPUT = -o $(output)
_EXE_SUFFIX =

ifperl Mpp::File::case_sensitive_filenames
  #
  # Uppercase C as a suffix is indistinguishable from a lowercase C on a case-insensitive
  # file system.
  #
  _CPP_SUFFIXES += C
endif
ifperl Mpp::is_windows

  iftrue $(filter %cl %cl.exe %bcc32 %bcc32.exe %fl %fl.exe, $(CC) $(CXX) $(FC))
    _OBJ_SUFFIX = .obj
    _OUTPUT =
  endif

  ifntrue $(makepp_no_builtin_linker)
    _EXE_SUFFIX = .exe
    #
    # We want "makepp xyz" to make xyz.exe if this is Windows.
    #
    $(basename $(foreach)): : foreach *.exe
	$(_exe_magic_)
  endif
endif

ifntrue $(makepp_no_builtin_linker)
  #
  # Link commands
  # We could split this up into keywords/builtins common to all Shells
  # and a "may have to type" list for others like "alias".
  #
  $(basename $(foreach))$(_EXE_SUFFIX): $(infer_objects $(foreach), *$(_OBJ_SUFFIX))
	:foreach $(wildcard $( bg break case cd chdir continue do done echo elif else \
		esac eval exec exit export fg fi for getopts hash if jobs kill login \
		newgrp pwd read readonly return set shift stop suspend test then \
		times trap type ulimit umask unset until wait while)$(_OBJ_SUFFIX))
	$(infer_linker $(inputs)) $(inputs) $(LDLIBS) $(LDFLAGS) $(LIBS) $(_OUTPUT)
	noecho makeperl {{
	  warn "On Unix, to run a program called `$(basename $(foreach))', you usually must type\n" .
	    "  ./$(basename $(foreach))\n" .
	    "rather than just `$(basename $(foreach))'.\n";
	}}

  $(basename $(foreach))$(_EXE_SUFFIX): $(infer_objects $(foreach), *$(_OBJ_SUFFIX))
	:foreach *$(_OBJ_SUFFIX)
	$(infer_linker $(inputs)) $(inputs) $(LDLIBS) $(LDFLAGS) $(LIBS) $(_OUTPUT)
endif

#
# C++ compilation:
#
iftrue $(makepp_percent_subdirs)
$(basename $(foreach))$(_OBJ_SUFFIX) : $(foreach) : foreach **/*.$(_CPP_SUFFIXES)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) $(_OUTPUT)
else
$(basename $(foreach))$(_OBJ_SUFFIX) : $(foreach) : foreach *.$(_CPP_SUFFIXES)
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c $(input) $(_OUTPUT)
endif

#
# C compilation:
#
%$(_OBJ_SUFFIX): %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) $(_OUTPUT)

#
# Fortran compilation:
#
%$(_OBJ_SUFFIX): %.f
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $(input) $(_OUTPUT)

#
# The rules for yacc and lex are marked :build_check target_newer because we
# don't want to reexecute them if the file already exists.  Some systems don't
# have yacc or lex installed, and some makefiles (e.g., the KDE makefiles)
# don't have a proper yacc/lex rule but run yacc/lex as part of the action of
# a phony target.
#

#
# Yacc:
#
%.c: %.y
	: build_check target_newer
	$(YACC) $(YFLAGS) $(input)
	&mv y.tab.c $(output)

#
# Lex:
#
%.c: %.l
	: build_check target_newer
	$(LEX) $(LFLAGS) -t $(input)
	&mv lex.yy.c $(output)
