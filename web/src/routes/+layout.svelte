<script lang="ts">
	import type { Snippet } from 'svelte';
	import { page } from '$app/state';

	import '$lib/css/reset.css';
	import '$lib/css/global.css';
	import Nav from '$lib/components/Nav.svelte';
	interface Props {
		children?: Snippet;
	}

	let { children }: Props = $props();

	let isIndexPage = $derived(page.url.pathname === '/');
</script>

{#if isIndexPage}
	<div class="float-nav">
		<Nav hideLogo={true} />
	</div>
{:else}
	<Nav />
{/if}

{@render children?.()}

<style>
	.float-nav {
		position: absolute;
    width: 100%;
		top: 0;
		left: 0;
    z-index: 1;
	}
</style>
