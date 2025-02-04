--wyrażenia



roots :: (Double,Double,Double)->(Double,Double)
roots (a, b, c)=
    let d = sqrt(b*b-4*a*c)
        e = 2*a
    in ((-b - d )/e,(-b + d)/e)

unitVec2D :: (Double,Double)->(Double,Double)
unitVec2D (x,y)=
    let len=sqrt(x**2+y**2)
    in (x/len,y/len)

unitVec3D :: (Double,Double,Double)->(Double,Double,Double)
unitVec3D (x,y,z)=
    let len=sqrt(x**2+y**2+z**2)
    in (x/len,y/len,z/len)

areaT :: (Double,Double,Double)->Double
areaT (a,b,c)=
    let p=(a+b+c)/2
    in sqrt(p*(p-a)*(p-b)*(p-c) )
    
