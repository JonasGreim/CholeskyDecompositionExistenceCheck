program testPositivDefinit
    implicit none
    integer , parameter:: columnRowLength = 4  ! Initialize how big the square matrix of the text file is
    ! also bigger or smaller matrices are possible
    integer, dimension(columnRowLength , columnRowLength) :: matrix
    logical :: isSymmetric = .TRUE.
    logical :: isPositivDefinit = .TRUE.
    INTEGER :: solutionDeterminat

    ! initialize the matrix from the text file
	
	! choose/uncomment one of the 3 matrix options
    ! open(10,file='CholeskyNotPossible.txt')
	
     open(10,file='CholeskyPossible.txt')

    ! -> Matrix columnRowLength have to be the same as in the text file -> here columnRowLength = 5
    ! open(10,file='5x5Matrix.txt')

    read (10,*) matrix

    ! first requirement for Cholesky: symmetry
    call checkSymmetry(columnRowLength, matrix, isSymmetric) ! returns all parameters (isSymmetric => True or False)

    ! second requirement for Cholesky: positiv determinat (here calculated by Laplace expansion)
    solutionDeterminat = determinatLaplace( matrix, columnRowLength )
    print *, ' '
    print *, 'The solution of Laplace expansion of the given Matrix is: '
    print *, solutionDeterminat
    call checkPositivDefiniteLaPlaceExpansion(columnRowLength, matrix, isPositivDefinit, isSymmetric)


! If the matrix is symmetric and the determinat > 0 (with Laplace expansion) then the matrix is positiv definit => then there is a Cholesky decomposition
    IF (isPositivDefinit) THEN
    print *, ' '
        print *, 'calculate Cholesky decomposition'
    end if

contains

subroutine printMatrix(columnRowLength, matrix)
    !printing given matrices

    integer :: i,j, columnRowLength
    integer, dimension(columnRowLength , columnRowLength) :: matrix

    do i = 1,columnRowLength
            print*, (matrix(i, j), j=1,columnRowLength)
    end do
end subroutine printMatrix

subroutine checkSymmetry(columnRowLength, matrix, isSymmetric)
! checks if the matrix is symmetric

        integer :: columnRowLength
        integer, dimension(columnRowLength , columnRowLength) :: matrix, matrixTransposed ! create two quadratic matrices with given length
        integer :: i, j ! loop helpers
        logical :: isSymmetric ! setter for solution


        matrixTransposed = transpose(matrix)	! function that transposes the matrix

        !printing both matrices
        print*,"----------------matrix---------------"
        call printMatrix(columnRowLength, matrix)

        print *, ' '

        print*,"--------transpose of matrix ---------"
        call printMatrix(columnRowLength, matrixTransposed)

        ! checking symmetric (compare tranpose and normal matrices)
        do i = 1, columnRowLength
            do j = 1, columnRowLength
                if(matrix(i,j) /= matrixTransposed(i,j)) then
                    isSymmetric = .FALSE.
                    exit		! exit loop and set solution of the subroutine: isSymmetric = false
                end if
            end do
        end do


        if (isSymmetric) then
            print*, "The matrix is equal to its transpose"
            print*,"Therfore, the matrix is symmetric"
         else
            print*, "The matrix is not equal to its transpose"
            print*, "Therefore, the matrix is antisymmetric"
        end if
    end subroutine checkSymmetry

subroutine checkPositivDefiniteLaPlaceExpansion(columnRowLength, matrix, isPositivDefinit, isSymmetric)
    integer::columnRowLength
    Integer, dimension(columnRowLength, columnRowLength) :: matrix
    logical :: isPositivDefinit, isSymmetric

    IF(solutionDeterminat>0 .AND. isSymmetric) THEN
        isPositivDefinit = .TRUE.
        print *, 'The given matrix is symmetric and the solution of the the Laplace expansion is greater than 0'
        print *, 'Therefore the matrix is positive definit and there is a Cholesky decomposition '
    Else
        isPositivDefinit = .FALSE.
        print *, 'The given matrix is not symmetric or the solution of the the Laplace expansion is not greater than 0'
        print *, 'Therefore there is not a Cholesky decomposition'
    END IF

end subroutine checkPositivDefiniteLaPlaceExpansion

recursive function determinatLaplace( matrix, n ) result( intermediateResult )
    integer :: n ! = columnRowLength (name here too long)
    integer:: matrix(n, n)
    integer:: submatrix(n-1, n-1), intermediateResult !intermediateResult
    integer :: i, sgn ! i=loop helper, sgn=sign for the change +/-

    if ( n == 1 ) then
        intermediateResult = matrix(1,1) ! The result of the determinats of a 1x1 matrix is its only element & calculates the recursion upwards again
    else
        intermediateResult = 0.0 ! reset for new tntermediateResult (last solution is safed in last recursion step)
        sgn = 1
        do i = 1, n ! go through first row //matrix(rows, columns)

            ! Sub-Array Manipulations
            ! Laplace expansion is made along the first row
            submatrix( 1:n-1, 1:i-1 ) = matrix( 2:n, 1:i-1 )! fill submatrix till i-1th column with matrix values that are between the first and i-1th column and not in the first row
            submatrix( 1:n-1, i:n-1 ) = matrix( 2:n, i+1:n )! fill submatrix from i column onwards with matrix values that are not in the first row and are between the i+1th column and n
            ! => skip first row & i-th column

            ! matrix(1,i) = value of first row elements -> alternately * +/- 1 (sgn)
            ! determinat method is called again with submatrix of the matrix with deleted 1st row and i-th column & the matrix columnRowlength-1
            ! the recursion calls end with the 1x1 matrices. After that the intermediateResult will get calculated up the recursion.
            ! one element of the first row fully calculated -> with do loop iterate through the hole row
            intermediateResult = intermediateResult + sgn * matrix(1, i) * determinatLaplace( submatrix, n-1 )
            sgn = - sgn ! change +/-
        enddo ! if do loop ends the intermediateResult is the solution
    endif
end function

end program testPositivDefinit


