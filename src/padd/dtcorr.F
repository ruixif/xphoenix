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
      Subroutine DTCORR(ICE,ICT1,ICT2,ICT3,NERR)
*
* Purpose :  Create correct energy scale from input data;
*            perform temperature corrections;
*            normalize input data.
* Date    :  March 5, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    ICE        = col. for energy
*    ICT1       = col. for temperature of first crystal
*    ICT2       = col. for temperature of second crystal
*    ICT3       = col. for temperature of analyzer crystal
*    DMX(*)     = energy, if ICE>0
*    DMA1(*)    = angle of 1. xtal, if ICA1>0
*    DMA2(*)    = angle of 2. xtal, if ICA2>0
*    DMT1(*)    = temperature of 1. xtal, if ICT1>0
*    DMT2(*)    = temperature of 2. xtal, if ICT2>0
*    DMT3(*)    = temperature of analyzer xtal, if ICT3>0
*    BPEAK      = if .FALSE. no peak fitting will be performed
*    DOPENG      = nuclear transition energy (keV)
*    DLNCS       = lattice constant (Angstroem), monochromator
*    DTHEX       = thermal expansion coeff. (1/K), monochromator
*    DTHEXA      = thermal expansion coeff. (1/K), analyzer
*    MILINX(*,1) = Miller indices of 1.crystal
*    MILINX(*,2) = Miller indices of 2.crystal
*
* export variables :
*    NERR       = error code
*    DMX(*)     = temperature corrected energy scale, in meV
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Dimension  DMTH(PDATA)
*
*:  reset errors
*
      NERR = 0
*
*:  properties of the crystal reflections
*
      DLAMB = 1.D-3*D2PI*DHQUER*DCLIC/DOPENG
      DG1   = Dsqrt(Dfloat(MILINX(1,1)**2+MILINX(2,1)**2+
     +                     MILINX(3,1)**2)) / DLCNS
      DG2   = Dsqrt(Dfloat(MILINX(1,2)**2+MILINX(2,2)**2+
     +                     MILINX(3,2)**2)) / DLCNS
      DSB1  = 0.5D0*DLAMB*DG1
      DSB2  = 0.5D0*DLAMB*DG2
*
      If(DSB1.gt.1.D0) Then
       NERR = 8801
       Return
      Endif
*
      If(DSB2.gt.1.D0) Then
       NERR = 8802
       Return
      Endif
*
      DTN1 = DSB1/Dsqrt((1.D0-DSB1)*(1.D0+DSB1))
      DTN2 = DSB2/Dsqrt((1.D0-DSB2)*(1.D0+DSB2))
*
*:  create energy scale
*
      If(ICE.eq.0) Then
       If(BPEAK) Then
        DEOFF = 0.D0
       Else
*
*:  determine center energy
*
        DAA    = Dasin(DSB1) + Dasin(DSB2) -
     +           (DMA1(KD/2) - DMA2(KD/2) - DE0P1 + DE0P2)*1.D-6
        DCAA   = Dcos(DAA)
        DOPEN9 = 5.D-4*D2PI*DHQUER*DCLIC/Dsin(DAA) *
     +     Dsqrt(DG1*(DG1+DG2*DCAA) + DG2*(DG2+DG1*DCAA))
        DEOFF  = 1.D6*(DOPEN9 - DOPENG)
*
*:  new Bragg angles
*
        DSB1 = DSB1*DOPENG/DOPEN9
        DSB2 = DSB2*DOPENG/DOPEN9
*
        If(DSB1.gt.1.D0) Then
         NERR = 8801
         Return
        Endif
*
        If(DSB2.gt.1.D0) Then
         NERR = 8802
         Return
        Endif
*
        DTN1   = DSB1/Dsqrt((1.D0-DSB1)*(1.D0+DSB1))
        DTN2   = DSB2/Dsqrt((1.D0-DSB2)*(1.D0+DSB2))
        DOPENG = DOPEN9
       Endif
*
       DFAC = DOPENG/(DTN1 + DTN2)
       DOFF = DMA1(KD/2) - DMA2(KD/2)
       Do  K = 1,KD
        DMX(K) = DFAC*(DMA1(K) - DMA2(K) - DOFF) + DEOFF
       EndDo
      Else
       DOFF = DMX(KD/2)
       Do  K = 1,KD
        DMX(K) = DMX(K) - DOFF
       EndDo
      Endif
*
*:  temperature correction for 1. crystal
*
      If(ICT1.ne.0) Then
       DFAC = 1.D6*DOPENG*DTHEX*DTN1/(DTN1 + DTN2)
*
*:  sudden change detection and removal
*
       If(DMT1(1).gt.200.D0) Then
        DSP = 1.D-5
       Else
        DSP = 1.D-4
       Endif
       D12 = Dabs(DMT1(1)-DMT1(2))
       D23 = Dabs(DMT1(2)-DMT1(3))
       D13 = Dabs(DMT1(1)-DMT1(3))
       If(D12.gt.D23/DSP .and. D13.gt.D23/DSP) Then
        DMT1(1) = 0.5D0*(DMT1(2) + DMT1(3))
       Endif
*
       Do  K = 2,KD
        If(Dabs(DMT1(K)-DMT1(K-1)).gt.DSP*Dabs(DMT1(K-1))) Then
         DMT1(K) = DMT1(K-1)
        Endif
       EndDo
*
*:  5-point average
*
       DMTH(1) = (DMT1(1)+DMT1(2)+DMT1(3)+DMT1(4)+DMT1(5))/5.D0
       DMTH(2) = DMTH(1)
       Do  K = 3,KD-2
        DMTH(K) = (DMT1(K-2)+DMT1(K-1)+DMT1(K)+
     +             DMT1(K+1)+DMT1(K+2))/5.D0
       EndDo
       DMTH(KD-1) = DMTH(KD-2)
       DMTH(KD)   = DMTH(KD-2)
*
       If(BPEAK) Then
        DOFF = DMTH(KD/2)
       Else
        DOFF = DE0T1
       Endif
       Do  K = 1,KD
        DMX(K) = DMX(K) - DFAC*(DMTH(K) - DOFF)
       EndDo
      Endif
*
*:  temperature correction for 2. crystal
*
      If(ICT2.NE.0) Then
       DFAC = 1.D6*DOPENG*DTHEX*DTN2/(DTN1 + DTN2)
*
*:  sudden change detection and removal
*
       If(DMT2(1).gt.200.D0) Then
        DSP = 1.D-5
       Else
        DSP = 1.D-4
       Endif
       D12 = Dabs(DMT2(1)-DMT2(2))
       D23 = Dabs(DMT2(2)-DMT2(3))
       D13 = Dabs(DMT2(1)-DMT2(3))
       If(D12.gt.D23/DSP .and. D13.gt.D23/DSP) Then
        DMT2(1) = 0.5D0*(DMT2(2)+DMT2(3))
       Endif
*
       Do  K = 2,KD
        If(Dabs(DMT2(K)-DMT2(K-1)).gt.DSP*Dabs(DMT2(K-1))) Then
         DMT2(K) = DMT2(K-1)
        Endif
       EndDo
*
*:  5-point average
*
       DMTH(1) = (DMT2(1)+DMT2(2)+DMT2(3)+DMT2(4)+DMT2(5))/5.D0
       DMTH(2) = DMTH(1)
       Do  K = 3,KD-2
        DMTH(K) = (DMT2(K-2)+DMT2(K-1)+DMT2(K)+
     +             DMT2(K+1)+DMT2(K+2))/5.D0
       EndDo
       DMTH(KD-1) = DMTH(KD-2)
       DMTH(KD)   = DMTH(KD-2)
*
       If(BPEAK) Then
        DOFF = DMTH(KD/2)
       Else
        DOFF = DE0T2
       Endif
       Do  K = 1,KD
        DMX(K) = DMX(K) - DFAC*(DMTH(K) - DOFF)
       EndDo
      Endif
*
*:  temperature correction for analyzer crystal
*
      If(ICT3.NE.0) Then
       DFAC = -1.D6*DOPENG*DTHEXA
*
*:  sudden change detection and removal
*
       If(DMT3(1).gt.200.D0) Then
        DSP = 1.D-5
       Else
        DSP = 1.D-4
       Endif
       D12 = Dabs(DMT3(1)-DMT3(2))
       D23 = Dabs(DMT3(2)-DMT3(3))
       D13 = Dabs(DMT3(1)-DMT3(3))
       If(D12.gt.D23/DSP .and. D13.gt.D23/DSP) Then
        DMT3(1) = 0.5D0*(DMT3(2)+DMT3(3))
       Endif
*
       Do  K = 2,KD
        If(Dabs(DMT3(K)-DMT3(K-1)).gt.DSP*Dabs(DMT3(K-1))) Then
         DMT3(K) = DMT3(K-1)
        Endif
       EndDo
*
*:  5-point average
*
       DMTH(1) = (DMT3(1)+DMT3(2)+DMT3(3)+DMT3(4)+DMT3(5))/5.D0
       DMTH(2) = DMTH(1)
       Do  K = 3,KD-2
        DMTH(K) = (DMT3(K-2)+DMT3(K-1)+DMT3(K)+
     +             DMT3(K+1)+DMT3(K+2))/5.D0
       EndDo
       DMTH(KD-1) = DMTH(KD-2)
       DMTH(KD)   = DMTH(KD-2)
*
       If(BPEAK) Then
        DOFF = DMTH(KD/2)
       Else
        DOFF = DE0TA
       Endif
       Do  K = 1,KD
        DMX(K) = DMX(K) - DFAC*(DMTH(K) - DOFF)
       EndDo
      Endif
*
*:  round energy steps to 0.01 meV
*:  apply energy scale corrections
*
      Do  K = 1,KD
       DMX(K) = (DENSCL + 1.D-6*DENQC*DMX(K))*DMX(K)
       DMX(K) = Dfloat(Idnint(DMX(K)*100.D0))/100.D0
      EndDo
*
*
      End
