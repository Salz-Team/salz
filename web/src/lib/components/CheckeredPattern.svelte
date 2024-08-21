<script lang="ts">
enum ColorChoice {
  Red = "red",
  Blue = "blue",
  Green = "green",
  Yellow = "yellow",
  Gold = "gold",
  Colorful = "colorful",
  Custom = "custom",
}

export let color: ColorChoice | string = ColorChoice.Red;
</script>

<svg
	version="1.1"
	viewBox="0 0 188 180"
	xmlns="http://www.w3.org/2000/svg"
	preserveAspectRatio="xMinYMin meet"
  class={`checkered-pattern ${color}`}
>
  <pattern id="tile" viewBox="0, 0, 10, 10" width=".25" height=".25" patternTransform="rotate(-30)">
		<rect width="5" height="5" class="main" />
		<rect x="5" width="5" height="5" class="sub" />
		<rect y="5" width="5" height="5" class="sub" />
		<rect x="5" y="5" width="5" height="5" class="main" />
	</pattern>

  <radialGradient id={`${color}-grad`} cx="50%" cy="50%" r="0.5">
    <stop opacity="1" offset="0" class="gradient-start-color" />
		<stop opacity="0" offset="100%" stop-color="white" class="gradient-stop-color" />
	</radialGradient>

  <radialGradient id="colorful" cx="50%" cy="50%" r="0.45">
    <stop opacity="1" offset="0" fill="#F17C58">
      <animate attributeName="stop-color" values="#F17C58;#E94584;#24AADB;#27DBB1;#FFDC18;#FF3706;#F17C58" dur="5s" repeatCount="indefinite" />
    </stop>
		<stop opacity="0" offset="100%" stop-color="white" />
  </radialGradient>

	<mask id="mask">
    <rect width="100%" height="100%" fill="url(#tile)" />
	</mask>

  {#if color !== ColorChoice.Colorful}
    <rect width="100%" height="100%" fill={`url(#${color}-grad)`} mask="url(#mask)" />
  {:else}
    <rect width="100%" height="100%" fill="url(#colorful)" mask="url(#mask)" />
  {/if}
</svg>

<style>
	svg {
		opacity: 0.7;
    width: 120px;
	}

  .gradient-stop-color {
    stop-color: white;
  }

  svg.colorful {
    opacity: 1;
  }

	.colorful .main {
		fill: black;
	}

	.colorful .sub {
		fill: rgb(255, 150, 150);
	}

  .colorful .gradient-start-color {
    stop-color: black;
  }

	.red .main {
		fill: red;
	}

	.red .sub {
		fill: rgb(255, 150, 150);
	}

  .red .gradient-start-color {
    stop-color: red;
  }

	.blue .main {
		fill: blue;
	}

	.blue .sub {
		fill: rgb(150, 150, 255);
	}

  .blue .gradient-start-color {
    stop-color: blue;
  }

	.green .main {
		fill: green;
	}

	.green .sub {
		fill: rgb(150, 255, 150);
	}

  .green .gradient-start-color {
    stop-color: green;
  }

	.yellow .main {
		fill: yellow;
	}

	.yellow .sub {
		fill: rgb(150, 255, 255);
	}

  .yellow .gradient-start-color {
    stop-color: yellow;
  }

	.gold .main {
		fill: gold;
	}

	.gold .sub {
		fill: rgb(252, 235, 136);
	}

  .gold .gradient-start-color {
    stop-color: gold;
  }

	.masking-polygon {
		transform-origin: 50% 50%;
		transform: scale(0.8) skew(-20deg);
	}
</style>
