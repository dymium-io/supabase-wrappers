import React, { useEffect } from 'react';
import { TweenLite, Circ } from "gsap";

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
    points = [];
    let angle = 3.14159 * 10 / 180
    cs = Math.cos(angle)
    sn = Math.sin(angle)
    let step = width / 18
    for (let i = 1; i <= 6; i++) {

      let rad = Math.abs(step * i)
      let xorig =  sn * step * i
      let yorig =  cs * step * i
      let drad = Math.random() * rad / 5
      let dx, dy
      if (i > 0) {
        dx = drad * sn
        dy = drad * cs
      } else {
        dx = -drad * sn
        dy = -drad * cs

      }

      let p = {
        originX: xorig, originY: yorig, originR: rad,
        drad, dx, dy, alpha: 0.5
      };

      points.push(p);

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

  // animation
  function initAnimation() {
    animate();
    for (let i in points) {
      shiftPoint(points[i]);
    }
  }

  function animate() {
    if (animateHeader) {
      ctx.clearRect(0, 0, width, height);
      for (let i in points) {

        draw(points[i]);
      }
    }
    requestAnimationFrame(animate);
  }

  function shiftPoint(p) {
    let drad = Math.random() * p.originR / 5
    let dx, dy

    dx = drad * sn
    dy = drad * cs

    TweenLite.to(p, 2 + 2 * Math.random(), {dx, dy, drad, alpha: 0.5 + 0.6*Math.random() - 0.3,
    ease: Circ.easeInOut,
      onComplete: function () {
        shiftPoint(p);
      }
    });
  }




  return (
    <canvas style={{ position: 'relative', top: '0px', left: '0px' }} id="anicanvas">

    </canvas>
  )
}

export default Backdrop;