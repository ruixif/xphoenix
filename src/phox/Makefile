#
##############################################################
#
#  PHOENIX Version 2.1.4
#  Copyright (C) 2016 Wolfgang Sturhahn
#
#  This file is part of PHOENIX-2.1.4.
#
#  PHOENIX-2.1.4 is free software:
#  You can redistribute it and/or modify it under the terms
#  of the GNU General Public License as published by the
#  Free Software Foundation
#
#  PHOENIX-2.1.4 is distributed in the hope that it will be
#  useful, but WITHOUT ANY WARRANTY; without even the implied
#  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE. See the GNU General Public License for details.
#
#  A copy of the GNU General Public License should have been
#  provided along with PHOENIX-2.1.4.
#  If not, see <http://www.gnu.org/licenses/>.
#
#
#  purpose :  create or update PHOENIX module phox
#  author  :  W.Sturhahn
#  created :  November 1, 1997
#
##############################################################
#

include ../../config/CONFIG
 
OBJS  = bgcorr.o cnstc.o  dbfit.o  dcnv.o   dfltr.o \
	errchk.o fts.o    gfit.o   lesdat.o lesphx.o \
	lesxrf.o mdfyrf.o moment.o phox.o   phxerr.o \
	putdsp.o putgrc.o savdat.o savhdr.o topipe.o

SRCS  = $(OBJS:.o=.F)

OUT   = ../../bin/phox-$(VERSION)

include ../../config/RULES

$(INCLUDE_DIR)/implici.h :
$(INCLUDE_DIR)/phoenix.h :
$(INCLUDE_DIR)/phox.h :
$(INCLUDE_DIR)/version.h :
$(INCLUDE_DIR)/xterm.h :

$(OBJS): $(INCLUDE_DIR)/implici.h
bgcorr.o fts.o lesdat.o lesphx.o lesxrf.o mdfyrf.o moment.o \
	phxerr.o putdsp.o putgrc.o savdat.o topipe.o: \
	$(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/phox.h
cnstc.o dcnv.o gfit.o: $(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/phox.h \
	$(INCLUDE_DIR)/xterm.h
phox.o: $(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/phox.h \
	$(INCLUDE_DIR)/version.h $(INCLUDE_DIR)/xterm.h

$(OUT): $(LIBRARY_DIR)/libphoenix-$(VERSION).a
