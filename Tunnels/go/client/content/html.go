package content

const Head = `<html>
		<head>
		<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/css/bootstrap.min.css" integrity="sha384-xOolHFLEh07PJGoPkLv1IbcEPTNtaed2xpHsD9ESMhqIYd0nLMwNLD69Npy4HI+N" crossorigin="anonymous">
		<script src="https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/js/bootstrap.min.js" integrity="sha384-+sLIOodYLS7CIrQpBjl+C7nPvqq+FbNUBDunl/OZv93DB7Ln/533i8e/mZXLi/P+" crossorigin="anonymous"></script>
		<style>
		#anicanvas {
			width: 100%;
			height: 100%;

			background: linear-gradient(-5deg, #015f81 0%, #0278a2 20%, rgb(0, 151,206) 50%, rgb(255,158,24) 50%, rgb(220, 137, 20)  80%, rgb(199, 124, 18) 100%);

			z-index: 0;
		  }
		#divbox {
			position: absolute;
			top: 10%;
			height: 60vh;
			z-index: 100;
			color: #7AFB4C;
			background: #000000;
			left: 15em; 
			right: 15em;
			text-align: left;
		}
		* { margin: 0; padding: 0; }
		.terminal {
			border-radius: 8px;
			position: relative;
			min-width: 300px;
		}
		.terminal .top {
			background: #E8E6E8;
			color: black;
			padding: 5px;
			border-radius: 8px 8px 0 0;
		}
		.terminal .btns {
			position: absolute;
			top: 7px;
			left: 5px;
		}
		.terminal .circle {
			width: 12px;
			height: 12px;
			display: inline-block;
			border-radius: 15px;
			margin-left: 2px;
			border-width: 1px;
			border-style: solid;
		}
		.title{
			text-align: center;
		}
		.red { background: #EC6A5F; border-color: #D04E42; }
		.green { background: #64CC57; border-color: #4EA73B; }
		.yellow{ background: #F5C04F; border-color: #D6A13D; }
		.clear{clear: both;}
		.terminal .body {
			background: black;
			color: #7AFB4C;
			padding: 8px;
			overflow: auto;
		}
		.space {
			margin: 25px;
		}
		.shade { 
			box-shadow: 0.3em 0.3em 1em rgba(0,0,0,0.5);;
		}
		.line { 
			padding-left: 4px; 
			padding-right: 4px; 
			font-face: monospace;
			font-size: 1.0em;
		}
		</style>

		</head>
		<body class="text-center">
		<canvas style={{ position: 'relative', top: '0px', left: '0px', z-index: '0' }} id="anicanvas">

		</canvas>
		
		<div id='divbox' class="terminal space shade">
		<div class="top">
        <div class="btns">
            <span class="circle red"></span>
            <span class="circle yellow"></span>
            <span class="circle green"></span>
        </div>
        <div class="title">bash -- 70x32</div>
    </div>
		<div class="line">OAuth2.0/OIDC exchange completed!</div>
		<div class="line">Sending authentication code to Dymium...</div>
		`

const Tail = `
		<div class="line">Secure tunnel successfully established!
		</div>
		<div class="line">Redirecting to Dymium portal...</div> 
		<script>
			window.setTimeout( () => {
				window.location.href="%sapi/datascopehelp" + "?token=%s&port=%d"
			}, 1500)
		</script>
			</div>
			</body>
			</html>		
			`
		
const ErrorTail = `
<div class="line">The tunnel was not established. <a href="https://portal.dymium.us">Please click here to troubleshoot.</a>

</div>
	</div>
	</body>
	</html>		
	`
			