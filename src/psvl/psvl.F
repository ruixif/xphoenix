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
*   Program PHOENIX module PSVL
*
* purpose :  To analyze the partial DOS and obtain
*             the Debye sound velocity
* date    :  April 2, 2006
* author  :  Wolfgang Sturhahn
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
* 
#include<phoenix.h>
#include<psvl.h>
#include<xterm.h>
*
      Character*1  X
      Character*4  XEXT
      Character*10 XD,XT,XZ
      Character*11 XB
      Character*72 XFILE
*
      Dimension RT(2),II(8)
*
*
*:  set version number
*
#include<version.h>
*
*:  set error code
*
      NERR = 0
*
*:  start global timer
*
      Call TIMEC(RT,1,1,-1)
*
*:  print copyright message
*
      Call COPYRD(6,XVERSION)
*
      Write(6,2000) XT_CYN,XT_DEF
 2000 Format(A,' -- PHOENIX module PSVL execution starting..',A/)
*
*:  obtain line arguments
*
      Call INSOPT(B,BGRAPH,BPRT,X,XFILE)
*
*:  date and time
*
      Call DATE_AND_TIME(XD,XT,XZ,II)
      XDATE = XD(5:6)//'/'//XD(7:8)//'/'//XD(3:4)
      XTIME = XT(1:2)//':'//XT(3:4)//':'//XT(5:6)
      XZONE = XZ(1:3)//':'//XZ(4:5)
*
*:  open input file
*
      Open(UNIT=LUNINP,FILE=XFILE,STATUS='OLD',IOSTAT=IERR,ERR=998)
*
*:  read the module specific input file
*
      Call LESPSV(NERR)
      Call ERRCHK(NERR,*999)
*
*:  close input file
*
      Close(LUNINP)
*
*:  read DOS data
*
      Call LESDAT(NERR)
      Call ERRCHK(NERR,*101)
  101 Continue
*
*:  open protocol file and write header
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_psvl_ptl.txt')
      Open(UNIT=LUNMES,FILE=XFILE,IOSTAT=IERR,ERR=998)
      Call HEADER(LUNMES,XVERSION,'psvl ')
*
*:  print info
*
      Write(6,2001) XT_MAG,XT_DEF,Idnint(DMASS)
      Write(LUNMES,2001) '','',Idnint(DMASS)
      If(DPRES.ne.0.D0) Then
       Write(6,2002) DPRES,DRHO,DDRHO,DKB,DDKB,DKBP,DDKBP,XEOS
       Write(LUNMES,2002) DPRES,DRHO,DDRHO,DKB,DDKB,DKBP,DDKBP,XEOS
      Else
       Write(6,2003) DRHO,DDRHO,DKB,DDKB
       Write(LUNMES,2003) DRHO,DDRHO,DKB,DDKB
      Endif
*
 2001 Format(/2X,A,'Input parameters -->',A/
     +       3X,'Mass number of resonant isotope :',I5)
 2002 Format(3X,'Pressure',24X,':',F7.1,' GPa'/
     + 3X,'Density at 0 GPa                :',F9.3,' +-',F7.3,' g/cm^3'/
     + 3X,'Adiabatic bulk modulus at 0 GPa :',F7.1,'   +-',F5.1,' GPa'/
     + 3X,'Pressure derivative at 0 GPa    :',F7.1,'   +-',F6.2/
     + 3X,'Equation of state type          :  ',A)
*
 2003 Format(3X,'Density',25X,':',F9.3,' +-',F7.3,' g/cm^3'/
     + 3X,'Adiabatic bulk modulus',10X,':',F7.1,'   +-',F5.1,' GPa')
*
*:  calculate density and bulk modulus from EOS
*
      If(DPRES.GE.0.1D0) Then
       MTYPE = 0
       If(XEOS.eq.'LO') MTYPE = 1
       If(XEOS.eq.'VI') MTYPE = 2
*
*:  density
*
       DPN    = DPRES/DKB
       DVV0   = DVEOS(DPN,DKBP,MTYPE)
       DVV0K  = 1.D3*(DVEOS(DPN/1.001D0,DKBP,MTYPE)-DVV0)/DVV0
       DVV0KP = 1.D3*(DVEOS(DPN,DKBP*1.001D0,MTYPE)-DVV0)/DVV0
       DRHO0  = DRHO
       DDRHO0 = DDRHO
       DDRHO  = Dsqrt((DDRHO0/DRHO0)**2 + (DVV0K*DDKB/DKB)**2 +
     +                          (DVV0KP*DDKBP/DKBP)**2)
       DRHO   = DRHO/DVV0
       DDRHO  = DRHO*DDRHO
*
*:  bulk modulus
*
       DKK0   = DKEOSP(DPN,DKBP,MTYPE)
       DKK0K  = 1.D3*(DKEOSP(DPN*1.001D0,DKBP,MTYPE)-DKK0)/DKK0
       DKK0KP = 1.D2*(DKEOSP(DPN,DKBP*1.01D0,MTYPE)-DKK0)/DKK0
       DKB0   = DKB
       DDKB0  = DDKB
*
       DKB    = DKB0*DKK0
       DDKB   = DKB*Dsqrt(((1.D0 + DKK0K)*DDKB0/DKB0)**2 +
     +               (DKK0KP*DDKBP/DKBP)**2)
*
*:  seismic velocity
*
       DH1    = 1.D0 + DKK0K + DVV0K
       DH2    = DKK0KP + DVV0KP
*
       DVB    = 1.D3*Dsqrt(DKB/DRHO)
       DDVB   = 0.5D0*DVB*Dsqrt((DH1*DDKB0/DKB0)**2 +
     +              (DDRHO0/DRHO0)**2 + (DH2*DDKBP/DKBP)**2)
*
       Write(6,2004) DRHO,DDRHO,DKB,DDKB
       Write(LUNMES,2004) DRHO,DDRHO,DKB,DDKB
      Else
       DVB  = 1.D3*Dsqrt(DKB/DRHO)
       DDVB = 0.5D0*DVB*Dsqrt((DDKB/DKB)**2 + (DDRHO/DRHO)**2)
      Endif
*
      Write(6,2005) DVB*1.D-3,DDVB*1.D-3
      Write(LUNMES,2005) DVB*1.D-3,DDVB*1.D-3
*
 2004 Format(3X,'Density',25X,':',F9.3,' +-',F8.3,' g/cm^3'/
     +   3X,'Adiabatic bulk modulus',10X,':',F7.1,'   +-',F6.1,' GPa')
 2005 Format(3X,'Bulk sound velocity',13X,':',F9.3,' +-',F8.3,' km/s'/)
*
*:  linearize x-scale of data
*
      Call BINEQ1(DMX,DMY,DSX,DSY,KD,KD,K,1)
      Call BINEQ1(DMX,DMDY,DSX,DSDY,KD,KD,K,5)
      Call DCOPY1(DSX,DMX,KD)
      Call DCOPY1(DSY,DMY,KD)
      Call DCOPY1(DSDY,DMDY,KD)
*
*:  create arrays for Debye sound velocity
*:  calculate average phonon energy
*
      DVOL = DMASS*DAMU/DRHO
      DN   = 1.D-4*(DVOL*1.5D0/DPI**2)**(1.D0/3.D0)/DHQUER
      Do  K = 1,KD
       If(DSY(K).le.0.D0 .or. DSX(K).le.0.D0) Then
        DSY(K)  = 0.D0
        DSDY(K) = 1.D2
       Else
        DSDY(K) = DSDY(K)/DSY(K)**2
        DSY(K)  = DSY(K)/DSX(K)**2
        DSY(K)  = DN/Dabs(DSY(K))**(1.D0/3.D0)
        DSDY(K) = DSY(K)*DSY(K)*DSDY(K)/9.D0
       Endif
      EndDo
*
*:  open fifo pipe and write data to it
*
      KVEL = Idnint(1.2D0*(DWS1+DWW2)/(DSX(2)-DSX(1)))
      KVEL = Max0(KVEL,2)
      KVEL = Min0(KVEL,KD)
*
      K1 = Idnint((DWS1-DSX(1))/(DSX(2)-DSX(1)))
      K1 = Min0(K1,KD)
      K1 = Max0(2,K1)
*
      If(BGRAPH) Then
       DSY(1)  = DSY(2)
       DSDY(1) = DSDY(2)
       Call TOPIPE(1,NERR)
       Call ERRCHK(NERR,*204)
  204  Continue
      Endif
*
*:  print info
*
      X  = '|'
      XB = XT_BLU//'|'//XT_DEF
      Write(6,2006) XT_MAG,XT_DEF,XB,XB,XB,XB,XB,XB,XB,XB,XB,
     +               XT_BLU,XT_DEF
      Write(LUNMES,2006) '','',X,X,X,X,X,X,X,X,X,'',''
 2006 Format(2X,A,'Results of fits to the partial DOS -->',A/
     +  5X,' fit range  ',A,'  v_D  ',A,'  v_P  ',A,
     +           '  v_S  ',A,' shear ',A,'  chi^2'/
     +  5X,'  (meV,nm)  ',A,' (m/s) ',A,' (m/s) ',A,
     +           ' (m/s) ',A,' (GPa) ',A,'|'/
     +  4X,'-------------+-------+-------+-------+-------+---------',A)
*
*:  open file for detailed fit reports
*
      XFILE = XOUT
      Call XMERGE(XFILE,'_vit.txt')
      Open(UNIT=LUNINP,FILE=XFILE,IOSTAT=IERR,ERR=999)
*
*:  calculate sound velocity from the density of states
*
      If(NWW.gt.1) Then
       DDWW = (DWW2 - DWW1)/Dfloat(NWW-1)
      Else
       DDWW = 0.D0
      Endif
*
      Do  I = 1,NWW
       DWW(I) = DWW1 + Dfloat(I-1)*DDWW
*
       K2 = Idnint((DWS1+DWW(I)-DSX(1))/(DSX(2)-DSX(1)))
       K2 = Min0(K2,KD)
       K2 = Max0(2,K2)
       If(Iabs(K2-K1).lt.5) Cycle
*
       Call VELSND(DSV,DDSV,DCHI,DQVAL,BCONV)
*
       DSVT(I) = DSV
*
*:  add density contribution to the error
*
       DDSVT(I) = DSV*Dsqrt((DDSV/DSV)**2 + (DDRHO/DRHO)**2/9.D0)
*
       KIX(I)   = K2
       DCHIT(I) = DCHI
       Do  K = 1,KIX(I)
        DTTY(K,I) = DTHRY(0,DSX(K),DP,0)
       EndDo
*
*:  send fit results to pipe
*
       If(BGRAPH) Then
        Call TOPIPE(I+1,NERR)
       Endif
*
*:  compressional and shear sound velocities
*
       Call DVEL(DVB,DSV,DVP,DVS)
*
       DSVPT(I) = DVP
       DSVST(I) = DVS
*
*:  errors
*
       If(DDVB.ne.0.D0) Then
        DVBP = DVB*1.01D0
        Call DVEL(DVBP,DSV,DVPP,DVSP)
        DDVP = (1.D2*(DVPP-DVP)*DDVB/DVB)**2
        DDVS = (1.D2*(DVSP-DVS)*DDVB/DVB)**2
       Else
        DDVP = 0.D0
        DDVS = 0.D0
       Endif
*
       If(DDSV.ne.0.D0) Then
        DSVP = DSV*1.01D0
        Call DVEL(DVB,DSVP,DVPP,DVSP)
        DDVP = DDVP + (1.D2*(DVPP-DVP)*DDSV/DSV)**2
        DDVS = DDVS + (1.D2*(DVSP-DVS)*DDSV/DSV)**2
       Endif
       DDVP = Dsqrt(DDVP)
       DDVS = Dsqrt(DDVS)
*
       DDSVPT(I) = DDVP
       DDSVST(I) = DDVS
*
       DG  = 1.D-6*DRHO*DVS**2
       DDG = 2.D0*DG*DDVS/DVS
*
       DWL1 = 4.136D-3*DTTY(K1,I)/DSX(K1)
       DWL2 = 4.136D-3*DTTY(K2,I)/DSX(K2)
*
       Write(6,2007) DSX(K1),DSX(K2),XB,Idnint(DSV),XB,
     +   Idnint(DVP),XB,Idnint(DVS),XB,DG,XB,DCHI
       Write(LUNMES,2007) DSX(K1),DSX(K2),X,Idnint(DSV),X,
     +   Idnint(DVP),X,Idnint(DVS),X,DG,X,DCHI
 2007  Format(5X,F4.1,' - ',F4.1,1X,A,1X,I5,1X,A,1X,I5,1X,A,1X,
     +            I5,1X,A,F6.1,1X,A,F7.2)
*
       If(BCONV) Then
        Write(6,2008) DWL1,DWL2,XB,Idnint(DDSV),XB,Idnint(DDVP),
     +                XB,Idnint(DDVS),XB,DDG,XB,''
        Write(LUNMES,2008) DWL1,DWL2,X,Idnint(DDSV),X,Idnint(DDVP),
     +                X,Idnint(DDVS),X,DDG,X,''
       Else
        Write(6,2008) DWL1,DWL2,XB,Idnint(DDSV),XB,Idnint(DDVP),
     +                XB,Idnint(DDVS),XB,DDG,XB,' no conv '
        Write(LUNMES,2008) DWL1,DWL2,X,Idnint(DDSV),X,Idnint(DDVP),
     +                X,Idnint(DDVS),X,DDG,X,' no conv '
       Endif
*
 2008  Format(5X,F4.1,' - ',F4.1,1X,A,2X,I4,1X,A,2X,I4,1X,A,2X,
     +           I4,1X,A,F6.1,1X,A,A)
*
      EndDo
*
      Close(LUNINP)
*
*:  close fifo pipe
*
      If(BGRAPH)  Call TOPIPE(-1,NERR)
*
*:  save data
*
      Call SAVDAT(NERR)
*
*:  list of created files
*
      If(Index(XOPT,',').eq.0) Then
       XEXT = '.dat'
      Else
       XEXT = '.csv'
      Endif
      L = LENX(XOUT,' ')
      Write(LUNMES,2009) XOUT(1:L)//'_psvl_ptl.txt',
     +   XOUT(1:L)//'_vit.txt',XOUT(1:L)//'_vel'//XEXT,
     +   XOUT(1:L)//'_vet'//XEXT,XOUT(1:L)//'_chi'//XEXT,
     +   XOUT(1:L)//'_vld'//XEXT,XOUT(1:L)//'_vlp'//XEXT,
     +   XOUT(1:L)//'_vls'//XEXT
 2009 Format(//2X,'Created files -->'//
     +       3X,'Protocol of calculations      : ',A/
     +       3X,'Velocity fit iterations       : ',A/
     +       3X,'Pseudo Debye sound velocity   : ',A/
     +       3X,'Pseudo Debye sound vel. fits  : ',A/
     +       3X,'Chi^2 values of fits          : ',A/
     +       3X,'Debye sound velocities        : ',A/
     +       3X,'P-wave sound velocities       : ',A/
     +       3X,'S-wave sound velocities       : ',A)
*
      Close(LUNMES)
*
      GoTo 999
*
*:  I/O error
*
  998 Continue
      NERR = 901
      XMSG = XFILE
      MSG1 = IERR
      Call ERRCHK(NERR,*999)
*
*:  print elapsed time
*
  999 Continue
      Call TIMEC(RT,1,1,0)
      Write(6,2010) XT_CYN,RT,XT_DEF
 2010 Format(
     +  /A,' -- CPU time :  user ',F6.2,' s  system ',F6.2,' s'/
     +   ' -- PHOENIX module PSVL finished',A/)
*
*
      End
