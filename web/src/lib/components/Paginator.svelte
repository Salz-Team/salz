<script lang="ts">
	import Button from '$lib/components/Button.svelte';
	import range from 'lodash/range';
	import PaginatorJumper from './PaginatorJumper.svelte';
	import {
		createSlidingIntInterval,
		type RangeSlider,
	} from '$lib/arcana/sliding-range-window.svelte';

	interface Props {
		class?: string;
		totalPages: number;
		currentPage?: number;
		maxPagesShown?: number;
	}

	let {
		class: className = '',
		totalPages,
		currentPage = $bindable(1),
		maxPagesShown = 5,
	}: Props = $props();

	let { leftOffset, rightOffset, rangeStart, rangeEnd }: RangeSlider = $derived(
    createSlidingIntInterval(currentPage, maxPagesShown, 2, totalPages - 1),
	);

	let jumpToPage = (min: number, max: number) => (n: number) => {
		currentPage = Math.min(Math.max(min, n), max);
	};
</script>

<nav class={className} class:hidden={totalPages === 1}>
	<Button class={currentPage === 1 ? 'active' : ''} type="button" onClick={() => (currentPage = 1)}>
		1
	</Button>

	{#if rangeStart > 2}
		<PaginatorJumper min={1} max={totalPages} onConfirm={jumpToPage(1, totalPages)} />
	{/if}

	{#each range(rangeStart, rangeEnd + 1) as i (i)}
		<Button
			class={currentPage === i ? 'active' : ''}
			type="button"
			onClick={() => (currentPage = i)}
		>
			{i}
		</Button>
	{/each}

	{#if rangeEnd + 1 != totalPages}
		<PaginatorJumper min={1} max={totalPages} onConfirm={jumpToPage(1, totalPages)} />
	{/if}

	<Button
		class={currentPage === totalPages ? 'active' : ''}
		type="button"
		onClick={() => (currentPage = totalPages)}
	>
		{totalPages}
	</Button>
</nav>

<style>
	nav.hidden {
		display: none;
	}

	nav :global(.transparent) {
		opacity: 0;
	}

	nav :global(button.active) {
		background: transparent;
		position: relative;
	}

	nav :global(button.active::before) {
		background: var(--accent-color);
		background-size: 400% 200%;
		animation: background-scroll 5s linear infinite;
		content: '';
		position: absolute;
		width: 100%;
		height: 100%;
		z-index: -2;
		top: 0;
		left: 0;
	}

	nav :global(button.active::after) {
		background: var(--colour-link-background);
		content: '';
		position: absolute;
		width: 90%;
		height: 90%;
		z-index: -1;
		top: 5%;
		left: 5%;
		clip-path: polygon(var(--slant-px) 0, 100% 0, calc(100% - var(--slant-px)) 100%, 0 100%);
	}

	nav :global(button:disabled) {
		opacity: 0;
	}
</style>
