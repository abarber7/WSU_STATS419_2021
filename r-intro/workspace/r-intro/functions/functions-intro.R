
# isWholeNumber ... camelCase
# WhiteSmith indentor ...
# isClose ...
# zeroIsh ... 

is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  
{
  # x = 1.3
  abs(x - round(x)) < tol;
}

# Alt-F-S
handShake = function(n=1, plotMe=FALSE)
{
  if(n < 1) { stop("n must be greater than 0"); }  # warning
  if(!is.wholenumber(n)) { stop("n must be an integer"); }
  h = n*(n-1)/2;
  # return( n*(n-1)/12 );
  if(plotMe)
  {
    q=n+10
    # can you loop through "n" points 
    x1=n:q
    y1= x1*(x1-1)/2
    
    # ... and connect "n-1" elements
    # for(i in 1:n) ... for(j in 1:(n-1))
    plot(x1,y1)
    q=n+9
    x1=n:q
    y1= x1*(x1-1)/2
    lines(x1, y1, col="red", lwd=2)
    
    # can you draw a circle
    plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="draw.circle")
    draw.circle(3,6,c(1,0.66,0.33),border="purple",
                col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
  }
  h;
}


# readBin
# readChar  ... one long string
# readLines ... vector of lines of strings 
read_orginal = function(){
    singleString <- paste(readLines("C:/_git_/github/datasets/declaration/draft.txt"), collapse=" ");
    return(singleString)
    }

read_unanimous = function(){
  line <- paste(readLines("C:/_git_/github/datasets/declaration/final.txt"), collapse=" ");
  return(line);
}

combine_drafts = function(){
  singleString <- paste(readLines("C:/_git_/github/datasets/declaration/draft.txt"), collapse=" ");
  line <- paste(readLines("C:/_git_/github/datasets/declaration/final.txt"), collapse=" ");
  blah = c(singleString, line)
  return(paste(blah, collapse = ""))
}

letter_counter = function(txtFile){
  aNum=0; bNum=0;cNum=0; dNum=0; eNum=0; fNum=0; gNum=0; hNum=0; iNum=0; jNum=0; kNum=0; lNum=0; mNum=0;
  nNum=0;oNum=0;pNum=0;qNum=0;rNum=0;sNum=0; tNum=0;uNum=0;vNum=0;wNum=0;xNum=0;yNum=0;zNum=0;bucket=0;
  txtstring_split <- strsplit(txtFile, "")[[1]]
  
  for ( ii in  txtstring_split ){
    if (ii == 'a'){
      aNum = aNum +1;
    } else if (ii=='b'){
      bNum = bNum +1;
    } else if (ii=='c'){
      cNum = cNum +1;
    } else if (ii=='d'){
      dNum = dNum +1;
    } else if (ii=='e'){
      eNum = eNum +1;
    } else if (ii=='f'){
      fNum = fNum +1;
    } else if (ii=='g'){
      gNum = gNum +1;
    } else if (ii=='h'){
      hNum = hNum +1;
    } else if (ii=='i'){
      iNum = iNum +1;
    } else if (ii=='j'){
      jNum = jNum +1;
    } else if (ii=='k'){
      kNum = kNum +1;
    } else if (ii=='l'){
      lNum = lNum +1;
    } else if (ii=='m'){
      mNum = mNum +1;
    } else if (ii=='n'){
      nNum = nNum +1;
    } else if (ii=='o'){
      oNum = oNum +1;
    } else if (ii=='p'){
      pNum = pNum +1;
    } else if (ii=='q'){
      qNum = qNum +1;
    } else if (ii=='r'){
      rNum = rNum +1;
    } else if (ii=='s'){
      sNum = sNum +1;
    } else if (ii=='t'){
      tNum = tNum +1;
    } else if (ii=='u'){
      uNum = uNum +1;
    } else if (ii=='v'){
      vNum = vNum +1;
    } else if (ii=='w'){
      wNum = wNum +1;
    } else if (ii=='x'){
      xNum = xNum +1;
    } else if (ii=='y'){
      yNum = yNum +1;
    } else if (ii=='z'){
      zNum = zNum +1;
    } else if (ii==' '){
      
    }  else {
      bucket = bucket +1;
    }
  }

   emp.data <- data.frame(
     a = c (aNum), b = c (bNum),c = c(cNum) ,d = c (dNum),e = c ( eNum),f = c ( fNum),g = c ( gNum),h = c ( hNum),
     i= c ( iNum),j = c ( jNum),k = c ( kNum),l = c ( lNum),m = c ( mNum),n = c (nNum), o = c (oNum),p = c (pNum),
     q= c (qNum), r = c (rNum),s = c (sNum),t = c ( tNum),u = c (uNum),v = c (vNum),w = c (wNum),x = c(xNum),y = c(yNum),z = c(zNum),OTHER= c(bucket),
     
     stringsAsFactors = FALSE
   )
   # Print the data frame.
   print(emp.data);
   barplot(t(as.matrix(emp.data)), beside=TRUE)
   axis(1, 1:28, LETTERS[1:28])
  
}



computeDeterminant = function(y= matrix)
{
  
  for(i in 1:nrow(y)){
    if (i == 1){
      a= y[1,1] * (( y[2,2] * y[3,3]) -(y[2,3]*y[3,2]))
    }
    if (i == 2){
      b= y[1,2] * (( y[2,1] * y[3,3]) -(y[2,3]*y[3,1]))
    }
    if (i == 3){
      c= y[1,3] * (( y[2,1] * y[3,2]) -(y[3,1]*y[2,2]))
    }
  }
  det = a -b +c
  print(det)
}