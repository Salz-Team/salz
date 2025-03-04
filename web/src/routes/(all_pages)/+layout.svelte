<script lang="ts">
	interface Props {
		children?: import('svelte').Snippet;
	}

	let { children }: Props = $props();
</script>

<div class="content-container">{@render children?.()}</div>

<style>
	.content-container {
		padding: 0 2em;
	}
</style>
