<script lang="ts">
	import Button from '$lib/components/Button.svelte';
	import type { Props } from '$lib/components/types/Paginator';

	let {
		class: className = '',
		offset = $bindable(0),
		limit = $bindable(20),
		total,
		hasMore,
	}: Props = $props();

	let onNext = () => {
		offset++;
	};

	let onPrev = () => {
		offset--;
	};
</script>

<nav class={className}>
	{#if offset > 0}
		<Button type="button" onClick={onPrev}>Prev</Button>
	{/if}
	Showing {offset * limit + 1} - {Math.min(total ?? Number.MAX_SAFE_INTEGER, (offset + 1) * limit)}
	{#if hasMore}
		<Button type="button" onClick={onNext}>Next</Button>
	{/if}
</nav>
