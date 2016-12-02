function pO2(TC,atm=760)
    
    if TC < 0 || TC > 40
        return println("Temp beyond valid model range 0-40 C") 
    end
    
    function HC(TC)
    return(-0.0000058333*TC^3+0.0001821*TC^2+0.072405*TC+2.5443)*10000
    end
    
    function vp(TC) 
    0.0456*TC^2-0.8559*TC+16.509
    end
    
    function coef(TC)
       if  TC<30 
            coef=.678
        else
            coef = .827
        end
        return coef
    end
    
    function adj(TC)
        
      if  TC<30 
            adj = 35
        else
            adj = 49
        end
        return adj  
        
    end
    
    function DO(TC,vp,ap=atm)
        
        return  ((ap-vp)*coef(TC))/(adj(TC)+TC)
          
    end
    
    return DO(TC,vp(TC),atm)*(1/1000)*(1/32)*(18/1000)*HC(TC)*atm
end
