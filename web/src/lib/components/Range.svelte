<script lang="ts">
	interface Props {
		value: number;
		max?: number;
		min?: number;
	}

	let { value = $bindable(0), max, min }: Props = $props();
</script>

<input type="range" bind:value {max} {min} />

<style>
	input[type='range'] {
    /* Resets */
		appearance: none;
		-webkit-appearance: none;
		width: 100%; /* Specific width is required for Firefox. */
		background: transparent; /* White by default in Chrome */

    /* Thumb parameters */
    --thumb-box-shadow:
			1px 1px 1px #000000,
			0px 0px 1px #0d0d0d;
    --thumb-border: 1px solid #000000;
    --thumb-height: 2rem;
    --thumb-width: 1rem;
    --thumb-border-radius: 3px;
    --thumb-background: #ffffff;
    --thumb-cursor: pointer;

    /* Track parameters */
    --track-width: 100%;
    --track-height: 8.4px;
    --track-cursor: pointer;
    --track-box-shadow: 1px 1px 1px #000000, 0px 0px 1px #0d0d0d;
    --track-background: var(--colour-darkgray);
    --track-border-radius: 1.3px;
    --track-border: 0.2px solid #010101;
	}

  :global(body.theme-light) input[type='range'] {
    --track-background: var(--colour-lightgray);
  }

  :global(body.theme-dark) input[type='range'] {
    --track-background: var(--colour-darkgray);
  }

  @media (prefers-color-scheme: light) {
    input[type='range'] {
      /* Track parameters */
      --track-background: var(--colour-lightgray);
    }
  }

  @media (prefers-color-scheme: dark) {
    input[type='range'] {
      /* Track parameters */
      --track-background: var(--colour-darkgray);
    }
  }

	input[type='range']::-webkit-slider-thumb {
		-webkit-appearance: none;

    box-shadow: var(--thumb-box-shadow);
		border: var(--thumb-border);
		height: var(--thumb-height);
		width: var(--thumb-width);
		border-radius: var(--thumb-border-radius);
		background: var(--thumb-background);
		cursor: var(--thumb-cursor);
	}

	input[type='range']::-moz-range-thumb {
    box-shadow: var(--thumb-box-shadow);
		border: var(--thumb-border);
		height: var(--thumb-height);
		width: var(--thumb-width);
		border-radius: var(--thumb-border-radius);
		background: var(--thumb-background);
		cursor: var(--thumb-cursor);
	}

	input[type='range']::-ms-thumb {
    box-shadow: var(--thumb-box-shadow);
		border: var(--thumb-border);
		height: var(--thumb-height);
		width: var(--thumb-width);
		border-radius: var(--thumb-border-radius);
		background: var(--thumb-background);
		cursor: var(--thumb-cursor);
	}

  input[type='range']::-moz-range-track {
    width: var(--track-width);
    height: var(-track-height);
    cursor: var(--track-cursor);
    box-shadow: var(--track-box-shadow);
    background: var(--track-background);
    border-radius: var(--track-border-radius);
    border: var(--track-border);
  }

	input[type='range']::-ms-track {
		width: 100%;
		cursor: pointer;

		/* Hides the slider so custom styles can be added */
		background: transparent;
		border-color: transparent;
		color: transparent;
	}
</style>
