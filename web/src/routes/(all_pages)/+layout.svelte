<script lang="ts">
	import Nav from '$lib/components/Nav.svelte';
	interface Props {
		children?: import('svelte').Snippet;
	}

	let { children }: Props = $props();
</script>

<Nav />
<div class="content-container">{@render children?.()}</div>

<style>
	.content-container {
		padding: 0 2em;
	}
</style>
