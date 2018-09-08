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
      Subroutine DRSEV(ND,NR,DM,DV,DEPS,NIT,NERR,MODE,DA)
*
* purpose :  Calculation of eigenvalues and eigenvectors of a
*            real symmetric matrix using Jacobi transformations;
*            this routine is faster than ECOMEI but requires
*            a real, symmetric input matrix.
* date    :  April 23, 1998
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    ND       = size of the used matrix arrays
*    NR       = rank of the matrix that is diagonalized (NR <= NDIM)
*    DM(*,*)  = matrix to be diagonalized (upper half used)
*    DV(*,*)  = approximate transformation matrix, used if MODE=1
*    DEPS     = average ratio of nondiagonal to diagonal
*               matrix elements for that iteration stops
*    NIT      = maximum allowed number of iterations
*    MODE     = if 1, then use DV(*,*) as a starting point
*    DA(*,*)  = workspace
*
* export variables :
*    DM(*,*)  = diagonalized matrix
*    DV(*,*)  = right side eigenvectors
*    NERR     = error code
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER( DCUT = 1.D-30 )
*
      DIMENSION  DV(ND,ND),DM(ND,ND),DA(ND,ND)
*
*:  set error code
*
      NERR = 0
*
*:  initialize
*
      IF(NR.LE.0) THEN
       NERR = 1
       RETURN
      ENDIF
*
      IF(NR.EQ.1) THEN
       DV(1,1) = 1.D0
       RETURN
      ENDIF
      N = NR
*
      IF(NIT.LE.0) THEN
       NERR = 2
       RETURN
      ENDIF
      NITER = NIT
*
*:  initial guess for eigenvector matrix DV
*:  if provided apply to input matrix
*
      IF(MODE.EQ.1) THEN
       CALL DMLPY2(DM,DV,DA,ND,N)
       CALL DCOPY2(DA,DM,ND,N)
       CALL DTRPS2(DV,DA,ND,N)
       CALL DMLPY2(DA,DM,DV,ND,N)
       CALL DCOPY2(DV,DM,ND,N)
       CALL DTRPS2(DA,DV,ND,N)
      ELSE
       DO 1  K = 1, N
        DO 2  I = 1, N
         DV(I,K) = 0.D0
    2   CONTINUE
        DV(K,K) = 1.D0
    1  CONTINUE
      ENDIF
*
*:  start the iteration loop
*
      DO 3  IT = 1, NITER
*
*:  calculate average absolute values for
*:  diagonal, off-diagonal, and all elements
*
       DOFF = 0.D0
       DIAG = 0.D0
       DO 4  I = 1, N
        DA(I,I) = DM(I,I)*DM(I,I)
        DIAG = DIAG + DA(I,I)
        DO 5  K = I+1, N
         DA(I,K) = DM(I,K)*DM(I,K)
         DA(K,I) = DA(I,K)
         DOFF    = DOFF + DA(I,K)
    5   CONTINUE
    4  CONTINUE
       DOFF = DOFF + DOFF
       DALL = DSQRT(DIAG+DOFF)/DFLOAT(N*N)
       DOFF = DSQRT(DOFF)/DFLOAT(N*(N-1))
       DIAG = DSQRT(DIAG)/DFLOAT(N)
       DTAU = DEPS*DIAG
*
*:  if the ratio of off-diagonal to diagonal
*:  is small enough, exit iteration
*:  this is the normal exit
*
       Write(6,'(7X,I3,7X,1PE12.3)') IT,DOFF/DALL
       IF(DOFF.LT.DTAU .OR. DALL.LT.DCUT) RETURN
*
*:  perform a series of elementary
*:  Jacobi transformations
*
       DTAUQ = DTAU*DTAU
*
       DO 6  J = 1, N/2
        DO 7  L = 1, 2*N+1-4*J
         M = MIN0(J+L,N+1-J)
         K = MAX0(J,L+3*J-N-1)
*
*:  if the off-diagonal matrix element (k,m) is
*:  small skip Jacobi transformation
*
         IF(DA(M,K)+DA(K,M) .LE. DTAUQ) GOTO 7
*
*:  calculate the elements of a Jacobi matrix
*
         DC  = DM(K,M) + DM(M,K)
         DD  = DM(K,K) - DM(M,M)
*
         IF(DC.EQ.0.D0 .AND. DD.EQ.0.D0) THEN
          DCX = 1.D0/DSQRT(2.D0)
          DSX = DCX
         ELSE
          EX  = CDSQRT(CDSQRT(DCMPLX(DD,DC)/DCMPLX(DD,-DC)))
          DCX = DREAL(EX)
          DSX = DIMAG(EX)
         ENDIF
*
*:  Jacobi rotation matrix
*
         D11 =  DCX
         D22 =  DCX
         D21 =  DSX
         D12 = -DSX
*
*:  transformation left side
*
         DO 9  I = 1, N
          DMKI    = DM(K,I)
          DMMI    = DM(M,I)
          DM(K,I) = D11*DMKI + D21*DMMI
          DM(M,I) = D12*DMKI + D22*DMMI
    9    CONTINUE
*
*:  transformation right side
*
         DO 10  I = 1, N
          DMIK    =  DM(I,K)
          DMIM    =  DM(I,M)
          DM(I,K) =  D22*DMIK - D12*DMIM
          DM(I,M) = -D21*DMIK + D11*DMIM
          DVIK    =  DV(I,K)
          DVIM    =  DV(I,M)
          DV(I,K) =  D22*DVIK - D12*DVIM
          DV(I,M) = -D21*DVIK + D11*DVIM
   10    CONTINUE
*
    7   CONTINUE
    6  CONTINUE
    3 CONTINUE
*
*:  we only come here if the maximum
*:  number of iterations was reached
*
      NERR     = 3
      DA(1,1) = DOFF
      DA(2,1) = DIAG
*
*
      END
