PROGRAM isolate_by_random

   INTEGER  :: isolate(7)
   INTEGER  :: temp
   INTEGER  :: count

   OPEN(unit=13,file="test_isolate.f90",status="unknown")

   CALL random_seed
   
   count=0
   
   DO j=1,7
      isolate(j)=0
   ENDDO

   outer:DO WHILE(.TRUE.)
     DO i=1,7
        CALL random_number(x)
        IF(x>=0.01)THEN
           temp=INT(x*100)
           IF(temp/=isolate(i))THEN
              isolate(i)=temp
              count=count+1
              IF(count>=7)EXIT outer
           ENDIF
        ENDIF
      ENDDO  
   ENDDO outer
   
   DO k=1,7
      WRITE(13,*)isolate(k)
      WRITE(*,*)isolate(k)
   ENDDO

STOP
END PROGRAM isolate_by_random