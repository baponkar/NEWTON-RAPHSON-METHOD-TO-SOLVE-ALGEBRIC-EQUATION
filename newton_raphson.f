      !NEWTON RAPHSON METHOD
      !TO SOLVE COSX=XEXP(X)
      PROGRAM NWTN_RAPHSON
       IMPLICIT NONE
       REAL::X(0:100),F,TEMP=0,DEL_F
       INTEGER::I,J,K
        DO I=1,100
          IF( F(TEMP)*F(TEMP+1) .LE. 0 )THEN
          PRINT*,"THE ROOT LIES IN BETWEEN ",TEMP," AND",TEMP+1
          EXIT
          ENDIF
          TEMP=TEMP+1
        END DO
         X(0)=TEMP
         
         DO I=0,100
          X(I+1)=X(I)+ F(X(I))/DEL_F(X(I))
          END DO


          PRINT*,"THE ROOT IS:",X(100)
          !PAUSE
       END PROGRAM NWTN_RAPHSON
       
       
       REAL FUNCTION F(X)
        IMPLICIT NONE
        REAL,INTENT(IN)::X
        F=X*COS(X)-X*EXP(X)
        !PAUSE
        END FUNCTION F

        REAL FUNCTION DEL_F(X)
        IMPLICIT NONE
        REAL,INTENT(IN)::X
        DEL_F=COS(X)-X*SIN(X)-EXP(X)-X*EXP(X)
        PAUSE
        END FUNCTION DEL_F
