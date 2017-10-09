use std::env;

fn hc(x: f64) -> f64 {
let hcc =(-0.0000058333*x.powf(3.)+0.0001821*x.powf(2.)+0.072405*x+2.5443)*10000.0;
//println!("hc: {}",hcc);
return hcc;
}

fn vp(x: f64) -> f64 {
let vpp = 0.0456*x.powf(2.)-(0.8559*x)+16.509;
//println!("vp: {}",vpp);
return vpp
}


fn dd(tc: f64,vp: f64,ap: f64)-> f64{
    let coef;
    let adj;
    if tc <30.0 {
    coef=0.678;
    adj = 35.0;
    }else{
    coef=0.827;
    adj = 49.0;
    }
   // println!("coef: {}",coef);
    //println!("adj: {}",adj);
    let ddd= ((ap-vp)*coef)/(adj+tc);
  //  println!("DO: {}",ddd);
    return ddd;
  }


fn main(){
let args: Vec<String> = env::args().collect();
let ref tc0=args[1];
let tc: f64 = tc0.parse().unwrap();
let vvp = vp(tc);
let atm = 760.0;
let ddo = dd(tc,vvp,atm);
let hhc=hc(tc);
println!("pO2: {}",ddo*(1./1000.)*(1./32.)*(18./1000.)*hhc*atm);
}
