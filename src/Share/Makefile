#
###############################################################
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
#  purpose :  create or update PHOENIX library
#  author  :  W.Sturhahn
#  created :  February 15, 1998
#
##############################################################
#

include ../../config/CONFIG

OBJS  = bineq1.o cread.o  d_rand.o dcopy1.o dcopy2.o dinvgm.o diverf.o \
	dlgs.o   dmlpy2.o dnorm1.o drsev.o  drunav.o dspll.o \
	dsplq.o  dtrps2.o efft.o   lenx.o   readppm.o xadj.o \
	xbase.o  xbase1.o xfill.o  xintpr.o xmerge.o xparse.o \
	xrmel.o  xstrip.o xtoup.o  timec.o \
	copyrd.o dth.o header.o insopt.o ticks.o

SRCS  = $(OBJS:.o=.F)

LIB   = ../../lib/libphoenix-$(VERSION).a

include ../../config/RULES

$(INCLUDE_DIR)/implici.h :

$(OBJS) : $(INCLUDE_DIR)/implici.h
