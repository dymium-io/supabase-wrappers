import React, { useEffect } from 'react';

function Backdrop() {
  let width, height, canvas, ctx, points, target, animateHeader = true;
  //, cs, sn;
  let centerx, centery;
  // Similar to componentDidMount and componentDidUpdate:
  useEffect(() => {
    initHeader();
    initAnimation();
    addListeners();
  });
  // Main

  function initHeader() {
    width = window.innerWidth;
    height = window.innerHeight;
    centerx = width / 2
    centery = height / 2

    target = { x: centerx, y: centery };

    canvas = document.getElementById('anicanvas');
    canvas.width = width;
    canvas.height = height;
    ctx = canvas.getContext('2d');

    // create points
    points = []
    let angle = -3.14159 * 80 / 180
    //cs = Math.cos(angle)
    //sn = Math.sin(angle)
    let step = width / 18
    let fac = 0.3
    let cx = 260*fac
    let cy = -57*fac
    let rx=200*fac
    let ry =300*fac

     let boost = 1
    for(let i = 1; i < 8; i++) {     
        let p = {cx: cx*boost, cy:cy*boost, rx:rx*boost, ry:ry*boost, factor: 1.0, alpha: 0.3}
        points.push(p);
        boost = boost * 1.45
      
    }

    for(let i = 0; i < 50; i++) {
      shakeField()
    }

  }
  function draw(p) {

    //if (!_this.active) return;
    ctx.strokeStyle = 'rgb(255, 255, 255, ' + p.alpha;


    ctx.closePath();
    ctx.beginPath();
    ctx.ellipse(centerx + p.cx*p.factor, centery + p.cy*p.factor, p.rx*p.factor, p.ry*p.factor, -Math.PI*37/90, 0, 2 * Math.PI);
    ctx.stroke();    
    ctx.closePath();
    ctx.beginPath();
    ctx.ellipse(centerx - p.cx*p.factor, centery - p.cy*p.factor, p.rx*p.factor, p.ry*p.factor, -Math.PI*37/90, 0, 2 * Math.PI);
    ctx.stroke();    
    ctx.closePath();


  }

  // Event handling
  function addListeners() {
    //window.addEventListener('scroll', scrollCheck);
    window.addEventListener('resize', resize);
  }

  animateHeader = true;
  function scrollCheck() {
    if (document.body.scrollTop > height) animateHeader = false;
    else animateHeader = true;
  }

  function resize() {
    let width = window.innerWidth;
    let height = window.innerHeight;
    //largeHeader.style.height = height + 'px';
    canvas.width = width;
    canvas.height = height;
    animateHeader = true
    //alert(width + ", " + height)
    initHeader()
    initAnimation()
  }

  function  shakeField() {
    function  mv(p) {
      let factor = 1  + (Math.random()  - 0.5)*2.0

      factor = 0.003*factor + 0.997*p.factor

      let alpha =  0.3 + (Math.random() - 0.5) * 0.5
      alpha = 0.05*alpha + 0.95*p.alpha

      Object.assign(p,  { factor, alpha})
    }
    function avg(p0, p1) {
      Object.assign(p1,  { factor: (p0.factor*0.05 + 0.95*p1.factor), 
        alpha: (p0.alpha + p1.alpha)/2})

    }
    for (let i in points) {
      mv(points[i]);
    }    
     for(let i = 0; i < points.length - 1; i++) {
       avg(points[i], points[i+1])
     }
     for(let i = 0; i < points.length - 1; i++) {
      avg(points[i], points[i+1])
    }
    for(let i = 0; i < points.length - 1; i++) {
      avg(points[i], points[i+1])
    }
  }

  
   // animation
   function initAnimation() {
    animate(0);

  }
  function animate(timestamp) {
    if (animateHeader) {
      ctx.clearRect(0, 0, width, height);
      ctx.lineStyle = 'rgb(255, 255, 255, ' + points[0].alpha;

      ctx.globalAlpha = points[0].alpha;
      let dx = 373.6
      let dy = 533.0
      let l =  Math.sqrt(dx*dx + dy*dy)
      let c = dx / l
      let s = dy / l
      ctx.lineWidth = 4;
      
      ctx.beginPath();
      ctx.moveTo(centerx  -  2*l*c, centery -  2*l*s)
      ctx.lineTo( centerx + 2*l*c, centery + 2*l*s, points[0].alpha)
      ctx.stroke();    
      ctx.closePath();

      for (let i in points) {
        draw(points[i]);
      }
    }
    shakeField()
    requestAnimationFrame(animate);
     
  }


  return (
    <canvas style={{ position: 'relative', top: '0px', left: '0px' }} id="anicanvas">

    </canvas>
  )
}

export default Backdrop;