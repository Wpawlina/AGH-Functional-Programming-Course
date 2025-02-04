--where 

roots :: (Double,Double,Double)->(Double,Double)
roots (a,b,c)=((-b - d )/e,(-b + d)/e) 
    where d=sqrt(b*b-4*a*c)
          e=2*a


unitVec2D :: (Double,Double)->(Double,Double)
unitVec2D (x,y)=(x/len,y/len)
    where len=sqrt(x**2+y**2)


unitVec3D :: (Double,Double,Double)->(Double,Double,Double)
unitVec3D (x,y,z)=(x/len,y/len,z/len)
    where len=sqrt(x**2+y**2+z**2)

areaT :: (Double,Double,Double)->Double
areaT (a,b,c)=sqrt(p*(p-a)*(p-b)*(p-c))
    where p=(a+b+c)/2

