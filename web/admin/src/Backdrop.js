import React, { useEffect } from 'react';
import { TweenLite, Circ } from "gsap";

function Backdrop() {
    let width, height, canvas, ctx, points, target, animateHeader = true;
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
      target = { x: width / 2, y: height / 2 };
  
  
      canvas = document.getElementById('anicanvas');
      canvas.width = width;
      canvas.height = height;
      ctx = canvas.getContext('2d');
  
      // create points
      points = [];
      for (let x = 0; x < width; x = x + width / 5) {
        for (let y = 0; y < height; y = y + height / 5) {
          let px = x + Math.random() * width / 5;
          let py = y + Math.random() * height / 5;
          let p = { x: px, originX: px, y: py, originY: py };
          points.push(p);
        }
      }
  
      // for each point find the 5 closest points
      for (let i = 0; i < points.length; i++) {
        let closest = [];
        let p1 = points[i];
        for (let j = 0; j < points.length; j++) {
          let p2 = points[j]
          if (!(p1 === p2)) {
            let placed = false;
            for (let k = 0; k < 5; k++) {
              if (!placed) {
                if (closest[k] === undefined) {
                  closest[k] = p2;
                  placed = true;
                }
              }
            }
  
            for (let k = 0; k < 5; k++) {
              if (!placed) {
                if (getDistance(p1, p2) < getDistance(p1, closest[k])) {
                  closest[k] = p2;
                  placed = true;
                }
              }
            }
          }
        }
        p1.closest = closest;
      }
  
      // assign a circle to each point
      for (let i in points) {
        let c = new Circle(points[i], 4 + Math.random() * 4, 'rgba(255,255,255,0.3)');
        points[i].circle = c;
      }
    }
  
    // Event handling
    function addListeners() {
  
      //window.addEventListener('scroll', scrollCheck);
      window.addEventListener('resize', resize);
    }
  
  
  
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
          // detect points in range
          if (Math.abs(getDistance(target, points[i])) < 4000) {
            points[i].active = 0.3;
            points[i].circle.active = 0.6;
          } else if (Math.abs(getDistance(target, points[i])) < 20000) {
            points[i].active = 0.1;
            points[i].circle.active = 0.3;
          } else if (Math.abs(getDistance(target, points[i])) < 40000) {
            points[i].active = 0.2;
            points[i].circle.active = 0.3;
          } else {
            points[i].active = 0.2;
            points[i].circle.active = 0.3;
          }
  
          drawLines(points[i]);
          points[i].circle.draw();
        }
      }
      requestAnimationFrame(animate);
    }
  
    function shiftPoint(p) {
      TweenLite.to(p, 1 + 1 * Math.random(), {
        x: p.originX - 50 + Math.random() * 100,
        y: p.originY - 50 + Math.random() * 100, ease: Circ.easeInOut,
        onComplete: function () {
          shiftPoint(p);
        }
      });
    }
  
    // Canvas manipulation
    function drawLines(p) {
      if (!p.active) return;
      for (let i in p.closest) {
        ctx.beginPath();
        ctx.moveTo(p.x, p.y);
        ctx.lineTo(p.closest[i].x, p.closest[i].y);
        ctx.strokeStyle = 'rgba(156,217,249,' + p.active + ')';
        ctx.stroke();
      }
    }
  
    function Circle(pos, rad, color) {
      let _this = this;
  
      // constructor
      (function () {
        _this.pos = pos || null;
        _this.radius = rad || null;
        _this.color = color || null;
      })();
  
      this.draw = function () {
        if (!_this.active) return;
        ctx.beginPath();
        ctx.arc(_this.pos.x, _this.pos.y, _this.radius, 0, 2 * Math.PI, false);
        ctx.fillStyle = 'rgba(156,217,249,' + _this.active + ')';
        ctx.fill();
      };
    }
  
    // Util
    function getDistance(p1, p2) {
      return Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2);
    }
  
  
    return (
        <canvas style={{ position: 'relative', top: '0px', left: '0px' }} id="anicanvas">
  
        </canvas>
    )
  }
  
  export default Backdrop;