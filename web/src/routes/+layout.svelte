<script lang="ts">
	import '$lib/css/reset.css';
	import '$lib/css/global.css';
	interface Props {
		children?: import('svelte').Snippet;
	}

	let { children }: Props = $props();
</script>

{@render children?.()}
