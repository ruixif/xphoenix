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
* purpose :  define arrays and commom areas
* date    :  April 2, 2006
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
      Character*2  XEOS
      Character*6  XZONE
      Character*8  XDATE,XTIME
      Character*20 XVERSION
      Character*72 XMES,XOUT,XOPT,XOPT1,XOPT2,XMSG,XMSG1
*
      Dimension    DMX(PDATA),DMY(PDATA),DMDY(PDATA),
     +             DSX(PDATA),DSY(PDATA),DSDY(PDATA),
     +             DTTY(PDATA,PSVL),DWW(PSVL),DCHIT(PSVL),
     +             DSVT(PSVL),DDSVT(PSVL),
     +             DSVPT(PSVL),DDSVPT(PSVL),
     +             DSVST(PSVL),DDSVST(PSVL),
     +             DP(PFIT),DPMIN(PFIT),DPMAX(PFIT),
     +             DDPMAX(PFIT),DDP(PFIT),BFIT(PFIT),
     +             KIX(PSVL)
*
      Common/CDECN/  NWW,KD,KVEL,K1,K2,KIX,MSG1,MSG2,BFIT
*
      Common/CDECNR/ DWS1,DWW1,DWW2,DRHO,DDRHO,DMASS,
     +               DPRES,DKB,DDKB,DKBP,DDKBP,
     +               DMX,DMY,DMDY,DSX,DSY,DSDY,DTTY,DWW,DCHIT,
     +               DSVT,DDSVT,DSVPT,DDSVPT,DSVST,DDSVST,
     +               DP,DPMIN,DPMAX,DDPMAX,DDP,DAVPH
*
      Common/CDECNC/ XMES,XOUT,XOPT,XOPT1,XOPT2,XVERSION,XEOS,
     +               XDATE,XTIME,XZONE,XMSG,XMSG1
*
