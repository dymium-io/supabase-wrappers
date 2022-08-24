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
			top: 30%;
			left: 0px;
			width: 100%;
			height: 100vh;
			z-index: 100;
			text-shadow: 1px 1px #FEFEFE;
		}
		</style>

		</head>
		<body class="text-center">
		<canvas style={{ position: 'relative', top: '0px', left: '0px', z-index: '0' }} id="anicanvas">

		</canvas>
		<div id='divbox'>
		<h3 class="mt-5">Sending authentication code to Dymium...</h3>
		`

const Tail = `
		<h3 class="mt-5">Certificate obtained! Now establishing a secure tunnel...</h3>
			</div>
			</body>
			</html>		
			`
