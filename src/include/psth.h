*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
* purpose :  define arrays and common areas
* date    :  December 20, 2010
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
      Character*6  XZONE
      Character*8  XDATE,XTIME
      Character*20 XVERSION
      Character*72 XDOS,XOUT,XOPT,XMSG1,XMSG2
*
      Dimension    DMX(PDATA),DMY(PDATA),DMDY(PDATA),
     +             DSX(PFFT),DSY(PFFT,0:PORDER),DSDY(PDATA),
     +             DSYP(PFFT),DGFX(PFFT),DGFY(PFFT),DGFDY(PFFT),
     +             DETR(PTEMP),DDETR(PTEMP),DFLM(PTEMP),DDFLM(PTEMP),
     +             DFRE(PTEMP),DDFRE(PTEMP),DSPH(PTEMP),DDSPH(PTEMP),
     +             DKIN(PTEMP),DDKIN(PTEMP),DSYT(PFFT,PTEMP-1)
*
      Common/CDECN/  KD,KDG,K1,NTMP,NENG,KDF,NEXP,KCUT,JMAX,
     +               NCOM1,NCOM2,BPTL
*
      Common/CDECNR/ DMX,DMY,DMDY,DSX,DSY,DSDY,DSYP,DGFX,DGFY,DGFDY,
     +               DSYT,DETR,DDETR,DFLM,DDFLM,
     +               DFRE,DDFRE,DSPH,DDSPH,DKIN,DDKIN,
     +               DTMP1,DTMP2,DECOIL,DGAMMA,DENG1,DENG2,
     +               DFWHM,DASM,DSHP,DELPST,DINTCT,DBGRD
*
      Common/CDECNC/ XDOS,XOUT,XOPT,XVERSION,
     +               XDATE,XTIME,XZONE,XMSG1,XMSG2
*
