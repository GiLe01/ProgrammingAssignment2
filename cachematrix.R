#
# makeCacheMatrix	: create a cache
# cacheSolve		: use the cache
#

#
# INPUT : A matrix
# OUPUT : A cached object
# FUNCTION : Return an object in which the matrix is cached
#
# COMMENTS :
# 	To be used with cacheSolve
#
makeCacheMatrix <- function(mat = matrix(nrow=0,ncol=0)) {
		# Attributes of the MyMatrix Object
        # mat  : a (NxN) Matrix
        # imat : the inverse of mat
        imat <- NULL
		#print ("In MyMatrix : object created , INV mat set to NULL")
		
		#
        # Methods
        #
        
		# Setter&Getter for the Matrix object 
		set <- function(m) {
		    #print ("In MyMatrix : Set function, new value being assigned to x ")
		    #print ("and hence setting mean as NULL ")
		    mat <<- m
		    imat <<- NULL
		    }
		
        get <- function() {
			#print ("In MyMatrix : In Get object function, will return object")
			return(mat)
			}
        
		# Setter&Getter for the Inversed Matrix 
        setInv <- function(m) {
			#print("In MyMatrix : In setInv function , cache Inv Matrix")
			imat <<- solve(m)
			}
        
        getInv <- function() {
			#print ("In MyMatrix : In getInv function, will return the Inv Matrix")
			#print (imat)
				
			return(imat)
			}
		
		#print("In MyMatrix : list of public methods")
		# Export the public methods to the world
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


#
# INPUT : A matrix
# OUPUT : The inverse matrix
# FUNCTION : Return the inverse matrix from the cache
#            It compute it & store on first call and retrieve it on subsequent calls
#
# COMMENTS :
# Use the following R commands to check the function 
#> m1000 <- matrix(sample.int(1000,size=1000000,replace=TRUE), nrow=1000)
#> i1000 <- MyMatrix(m1000)
#> system.time(cacheInvMat(i1000))   <= around 5.30
#> system.time(cacheInvMat(i1000))   <= around 0
#
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        #message ("In cacheInvMat : ")
		mat <- m$getInv()
		
        if(is.null(mat)) {
        #    message("In cacheInvMat : Compute Inverse of the matrix and save in the cache")
            data <- m$get()
            mat <- solve(data, ...)
            m$setInv(mat)          
            }
		#else {		    
		#    message("In cacheInvMat : cached Matrix not NULL, returning cached Matrix....")    			
		#}
		
		return(mat)
}
