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
      Character*6   XZONE
      Character*8   XDATE,XTIME
      Character*20  XVERSION
      Character*72  XMES,XRES,XOUT,XOPT,XMSG,XMSG1,
     +              XDBG
      Real(8)          Iterator,OUTER
*
      Dimension    DMX(PDATA),DMY(PDATA),DMDY(PDATA),
     +             DSX(PDATA),DSY(PDATA),DSDY(PDATA),
     +             DDBX(PDATA),DDBY(PDATA),DDBDY(PDATA),
     +             DRY(PDATA),DRRX(PDATA),DRRY(PDATA),
     +             DCNY(PDATA,3),DCNDY(PDATA),DCY(PDATA),DCDY(PDATA),
     +             DQFCT(PDATA),DCOVAR(PDATA,PDATA),
     +             DP(PFIT),DDP(PFIT),
     +             DMD(0:PMN),DMR(0:PMN),DMS(0:PMN),DMSR(0:PMN)
*
      Common/CDECN/  KD,KMAX,K1,K2,KRD,KPD1,KPD2,KDBN,KDBP,KDBL,
     +               NF,NCOM1,NCOM2,BREXT,BPROC,BDCNV
*
      Common/CDECNR/ DMX,DMY,DMDY,DSX,DSY,DSDY,DDBX,DDBY,
     +               DDBDY,DRY,DRRX,DRRY,DQFCT,DCOVAR,
     +               DCNY,DCNDY,DCY,DCDY,
     +               DP,DDP,DMD,DMR,DMS,
     +               DMSR,DEC0,DEC1,DEC2,
     +               DVEC0,DVEC1,DVEC2,DVEC12,DCHI0,DCHI2,
     +               DSCLE,DFFAC,DKIN,DMPHE,DDFFAC,DFUDGE,
     +               DDKIN,DDMPHE,DBGR,DBGRD,DENSCL,
     +               DENQC,DASM,DPCF,DIFL,DRANGE,DCRNGE,DRNGE0,
     +               DRESCL,DRESCR,DRESMX,DRESBK,DPROC1,DPROC2,
     +               DBETA,DECOIL,DGAMMA,
     +               DTEMP,DTEMPSTEP,DTEMPEND,
     +               DDBGSTEP,DDBGEND
     
*
      Common/CDECNC/ XMES,XRES,XDBG,XOUT,XOPT,XVERSION,
     +               XDATE,XTIME,XZONE,XMSG,XMSG1
*
