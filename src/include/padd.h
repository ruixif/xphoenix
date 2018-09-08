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
* date    :  April 9, 1996
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
      Character*1   XCC
      Character*6   XZONE
      Character*8   XDATE,XTIME
      Character*20  XVERSION
      Character*72  XMES(PDS),XRES,XOUT,XOPT,XMSG1,XMSG2
      Character*200 XDIR,XPLGIN,XINTYP,XINFO(PDS)
*
      Dimension    DMX(PDATA),DMY(PDATA),DMDY(PDATA),DMZ(PDATA),
     +             DMDZ(PDATA),DSX(PDATA),DSY(PDATA),DSDY(PDATA),
     +             DRRX(PDATA),DRRY(PDATA),DMA1(PDATA),DMA2(PDATA),
     +             DMT1(PDATA),DMT2(PDATA),DMT3(PDATA),
     +             DMC1(PDATA),DMC2(PDATA),DMN(PDATA),DMM(PDATA),
     +             DMONAV(PDS),DTBH(PDS),DDTBH(PDS),DNTBH(PDS),
     +             DEVON(PDS),DEBIS(PDS),DTOTAL(0:PDS),DMSIDE(0:PDS),
     +             DFLC(PDS),DP(PFIT),DDP(PFIT),
     +             MILINX(3,2),NICS(PDS),BDS(PDS)
*
      Common/CDECN/  MILINX,NCL,KD,KDS,KMAX,K1,K2,KRD,KINTPL,
     +               NCOM1,NCOM2,NICS,BDS,BREXT,BPROC,BPEAK
*
      Common/CDECNR/ DMX,DMY,DMDY,DMZ,DMDZ,DSX,DSY,DSDY,DRRX,DRRY,
     +               DMA1,DMA2,DMT1,DMT2,DMT3,DMC1,DMC2,DMN,DMM,
     +               DFLC,DP,DDP,DZ,DBGRD,DTHEX,DTHEXA,DENSCL,
     +               DENQC,DENSHF,DBGR,DASM,DPCF,DIFL,DRANGE,DCRNGE,
     +               DTEMP,DOPENG,DLCNS,DMONAV,DTBH,DDTBH,DNTBH,
     +               DEVON,DEBIS,DTOTAL,DMSIDE,
     +               DE0P1,DE0T1,DE0P2,DE0T2,DROTD1,DROTD2,DE0TA,
     +               DA1E0,DC1E0,DT1E0,DA2E0,DC2E0,DT2E0,DT3E0,
     +               DRESCL,DRESCR,DRESMX,DRESBK,DPROC1,DPROC2
*
      Common/CDECNC/ XINFO,XMES,XRES,XOUT,XOPT,XCC,XVERSION,
     +               XINTYP,XPLGIN,XDIR,XDATE,XTIME,XZONE,
     +               XMSG1,XMSG2
*
