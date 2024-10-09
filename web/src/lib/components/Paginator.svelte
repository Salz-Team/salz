<script lang="ts">
	import Button from '$lib/components/Button.svelte';
	import range from 'lodash/range';

	interface Props {
		class?: string;
		totalPages: number;
		currentPage?: number;
		maxPagesShown?: number;
		noEdgeJumpers?: boolean;
	}

	let {
		class: className = '',
		totalPages,
		currentPage = $bindable(1),
		maxPagesShown = 7,
		noEdgeJumpers = false,
	}: Props = $props();

	let halfMaxPagesShown = maxPagesShown % 2 === 0 ? maxPagesShown / 2 : (maxPagesShown + 1) / 2;

	let rangeStart = $state(
		currentPage <= halfMaxPagesShown
			? 1
			: totalPages - currentPage < halfMaxPagesShown
				? totalPages - maxPagesShown + 1
				: currentPage - halfMaxPagesShown + 1,
	);
	let rangeEnd = $derived(Math.min(totalPages + 1, rangeStart + maxPagesShown));
</script>

<nav class={className} class:hidden={totalPages === 1}>
	<Button class={currentPage === 1 ? 'active' : ''} type="button" onClick={() => (currentPage = 1)}>
		1
	</Button>

	{#if currentPage - halfMaxPagesShown >= 1}
		<span>...</span>
	{/if}

	{#each range(rangeStart + 1, rangeEnd - 1) as i (i)}
		<Button
			class={currentPage === i ? 'active' : ''}
			type="button"
			onClick={() => (currentPage = i)}
		>
			{i}
		</Button>
	{/each}

	{#if currentPage + halfMaxPagesShown <= totalPages}
		<span>...</span>
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
		background: lightgray;
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
