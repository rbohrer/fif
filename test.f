      DIMENSION B(123)
      DIMENSION K(1,2,3)
 1    FORMAT (I3/F2.1)
 2    FORMAT (I3/1H /F2.1)
      K(1,1,1) = 5
      X =K(1,1,1)
      Y = -X
      Z = X + Y + (1 * (3 ** 2 + 3)  + 7)
      N = ---((122))
      W = 5. + MIN1F(X + 2., Y) - 4.
      READ 1 N, X
      PRINT 2 N, X
      IF (N - 5) 123, 34, 12
      ASSIGN 34 TO AA
      GO TO 123
      DO 123 I=3, 12
      GO TO AA (123, 34, 12)
      GO TO (123, 12) N
      DO 123 J=4, 6, 2
 123  CONTINUE
 34   CONTINUE
 12   CONTINUE
      STOP
