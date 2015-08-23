# 
# Copyright (c) Microsoft Corporation.   All rights reserved.
#

MAKESRCDIR=redist\core\HiRegion
BCMAKE=..\..\..
!include "$(BCMAKE)\makefile.bc"

# These are only separate so that bt can put output files in different places:

HiRegionAsm.cs: HiRegion.cs
	copy HiRegion.cs HiRegionAsm.cs
HiRegionWPAsm.cs: HiRegion.cs
	copy HiRegion.cs HiRegionWPAsm.cs

$(OBJDIR)\HiRegionAsm.exe: $(MSILOBJDIR)\HiRegionAsm.exe
	$(BC) /sepcomp /Fa $(BCFLAGS_START) $(LOG) $(MAIN) $** $(BCFLAGS_END)

$(OBJDIR)\HiRegionWPAsm.exe: $(MSILOBJDIR)\HiRegionWPAsm.exe
	$(BC) /Fa $(BCFLAGS_START) $(LOG) $(MAIN) $** $(BCFLAGS_END)
