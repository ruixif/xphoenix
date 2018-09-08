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
#  purpose :  create or update PHOENIX module pdsp
#  author  :  W.Sturhahn
#  created :  August 17, 2009
#
##############################################################
#

include ../../config/CONFIGgr

FILES = color frame line logo plot_error_bars padd \
	phox plot  psth  psvl text xaxis yaxis

OBJSL = $(addsuffix .o, $(addprefix dsp_,$(FILES)))

OBJS  = $(OBJSL) pdsp.o

SRCS  = $(OBJS:.o=.F)

OUT   = ../../bin/pdsp-$(VERSION)

include ../../config/RULES

$(INCLUDE_DIR)/implici.h :
$(INCLUDE_DIR)/phoenix.h :

$(OBJS) : $(INCLUDE_DIR)/implici.h
pdsp.o : $(INCLUDE_DIR)/phoenix.h
