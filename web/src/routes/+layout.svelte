<script lang="ts">
	import type { Snippet } from 'svelte';

	import '$lib/css/reset.css';
	import '$lib/css/global.css';
	interface Props {
		children?: Snippet;
	}

	let { children }: Props = $props();
</script>

{@render children?.()}
