possible <- function(x,y,n,grid){
  if(any(grid[x,] == n)){
    return(F)
  }
  if(any(grid[,y] == n)){
    return(F)
  }
  x0 <- floor((x-1)/3)+1
  y0 <- floor((y-1)/3)+1
  box <- grid[(3*x0-2):(3*x0),(3*y0-2):(3*y0)]
  if(any(box == n)){
    return(F)
  }
  return(T)
}
solve_sudoku <- function(grid,need = NULL,index = 1){
  if(is.null(need)){
    need <- which(grid == 0,arr.ind = T)
  }
  if(index > nrow(need)){
    result <<- grid
    print("Solved")
    return(T)
  }else{
    x <- need[index,1]
    y <- need[index,2]
  }
  for(i in 1:9){
    if(!possible(x,y,i,grid)){
      next
    }else{
      board2 <- grid
      board2[x,y] <- i
      if(solve_sudoku(board2,need,index+1)){
        return(result)
      }
    }
  }
  return(F)
}