import React, { useEffect } from 'react';

function Backdrop() {
  let width, height, canvas, ctx, points, target, animateHeader = true, cs, sn;
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
    let angle = 3.14159 * 10 / 180
    cs = Math.cos(angle)
    sn = Math.sin(angle)
    let step = width / 18

    for (let i = 2; i <= 10; i++) {

      let rad = Math.abs(step * i*i/6)
      let xorig =  sn * step * i*i/6
      let yorig =  cs * step * i*i/6

      let drad = Math.random() * rad * 4
      let dx = drad * sn
      let dy = drad * cs

      let p = {
        originX: xorig, originY: yorig, originR: rad,
        drad, dx, dy, alpha: 0.5
      };

      points.push(p);
    }
    for(let i = 0; i < 50; i++) {
      shakeField()
    }

  }
  function draw(p) {

    //if (!_this.active) return;
    ctx.strokeStyle = 'rgb(255, 255, 255, ' + p.alpha;
    ctx.beginPath();
    ctx.arc(centerx + p.originX + p.dx, centery + p.originY + p.dy, p.originR + p.drad, 0, 2 * Math.PI, false);
    ctx.closePath();
    ctx.stroke();

    ctx.beginPath();
    ctx.arc(centerx - p.originX - p.dx, centery - p.originY - p.dy, p.originR + p.drad, 0, 2 * Math.PI, false);
    ctx.closePath();
    ctx.stroke();

    //ctx.fillStyle = 'rgba(156,217,249,' + _this.active + ')';
    //ctx.fill();

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
      let dx, dy

      let drad = Math.random() * p.originR * 4
      dx = drad * sn
      dy = drad * cs

      drad = 0.005*drad + 0.995*p.drad


      dx = 0.005*dx + 0.995*p.dx
      dy = 0.005*dy + 0.995*p.dy

      let alpha = Math.random() 
      alpha = 0.1*alpha + 0.9*p.alpha

      Object.assign(p,  { dx, dy, drad, alpha})
    }
    function avg(p0, p1) {
      Object.assign(p1,  { dx: (p0.dx + p1.dx)/2, 
        dy: (p0.dy + p1.dy)/2, drad: (p0.drad + p1.drad)/2, alpha: (p0.alpha + p1.alpha)/2})

    }
    for (let i in points) {
      mv(points[i]);
    }    
     for(let i = 0; i < points.length - 1; i++) {
       avg(points[i], points[i+1])
     }
  }
  // animation
  function initAnimation() {
    animate();

  }
  
  function animate(timestamp) {
    if (animateHeader) {
      ctx.clearRect(0, 0, width, height);
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