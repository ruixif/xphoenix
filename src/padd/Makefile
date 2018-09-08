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
#  purpose :  create or update PHOENIX module padd
#  author  :  W.Sturhahn
#  created :  November 1, 1997
#
##############################################################
#

include ../../config/CONFIG

OBJS  = anashf.o angcal.o api.o    ceval.o  dtcorr.o errchk.o \
	filexp.o fmtrd.o  gfit.o   intadd.o lespad.o lesxrf.o \
	mdfyrf.o newxsc.o nrmdat.o padd.o   paderr.o prodat.o \
	putdsp.o putgrc.o resout.o savhdr.o savmon.o savnrm.o \
	savshf.o savsum.o topipe.o

SRCS  = $(OBJS:.o=.F)

OUT   = ../../bin/padd-$(VERSION)

include ../../config/RULES

$(INCLUDE_DIR)/implici.h :
$(INCLUDE_DIR)/phoenix.h :
$(INCLUDE_DIR)/padd.h :
$(INCLUDE_DIR)/version.h :
$(INCLUDE_DIR)/xterm.h :

$(OBJS): $(INCLUDE_DIR)/implici.h
ceval.o: $(INCLUDE_DIR)/phoenix.h
anashf.o angcal.o api.o dtcorr.o filexp.o fmtrd.o intadd.o \
	lespad.o lesxrf.o mdfyrf.o newxsc.o nrmdat.o paderr.o \
	putdsp.o putgrc.o savmon.o savnrm.o savshf.o \
	savsum.o topipe.o: \
	$(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/padd.h
gfit.o prodat.o resout.o: $(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/padd.h \
	$(INCLUDE_DIR)/xterm.h
padd.o: $(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/padd.h \
	$(INCLUDE_DIR)/version.h $(INCLUDE_DIR)/xterm.h
