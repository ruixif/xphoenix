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
#  purpose :  create or update PHOENIX module psvl
#  author  :  W.Sturhahn
#  created :  August 14, 2009
#
##############################################################
#

include ../../config/CONFIG

OBJS  = dkeosp.o dkeosv.o dpeos.o  dthry.o  dvel.o dveos.o \
	errchk.o lesdat.o lespsv.o psverr.o psvl.o putdsp.o \
	putgrc.o savdat.o savhdr.o topipe.o velsnd.o vfit.o

SRCS  = $(OBJS:.o=.F)

OUT   = ../../bin/psvl-$(VERSION)

include ../../config/RULES

$(INCLUDE_DIR)/implici.h :
$(INCLUDE_DIR)/phoenix.h :
$(INCLUDE_DIR)/psvl.h :
$(INCLUDE_DIR)/version.h :
$(INCLUDE_DIR)/xterm.h :

$(OBJS) : $(INCLUDE_DIR)/implici.h
psvl.o : $(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/psvl.h \
	$(INCLUDE_DIR)/version.h $(INCLUDE_DIR)/xterm.h 
lesdat.o lespsv.o psverr.o putdsp.o putgrc.o savdat.o savhdr.o \
topipe.o velsnd.o : \
	$(INCLUDE_DIR)/phoenix.h $(INCLUDE_DIR)/psvl.h
