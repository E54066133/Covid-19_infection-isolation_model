PROGRAM edge

    REAL     :: infection_edge(100)
    INTEGER  :: temp_int


    CALL random_seed
    OPEN(unit=13,file="edge_data.f90",status="unknown")

    DO i=1,100
       CALL random_number(x)
       temp_int=int(x*10)
       infection_edge(i)= real(temp_int)*0.1
    ENDDO


    DO j=1,100
       WRITE(13,*)infection_edge(j)
    ENDDO


STOP
END PROGRAM edge