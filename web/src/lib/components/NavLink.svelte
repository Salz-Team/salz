<script lang="ts">
	import { page } from '$app/stores';

	export let href: string;
	export let canBeActivated: boolean = true;
	export let useExactPath: boolean = false;

	$: shouldBeActive =
		canBeActivated &&
		(!useExactPath ? $page.url.pathname.startsWith(href) : $page.url.pathname === href);
</script>

<li class="navlink" class:active={shouldBeActive}>
	<a {href}>
		<slot></slot>
	</a>
</li>

<style>
	a:link,
	a:visited {
		text-decoration: none;
		color: inherit;
	}

	a:link {
		--slant-px: 1rem;
		padding: 0.6em 1em;
		display: inline-flex;
		align-items: center;
		justify-content: center;
		background: lightgray;
		transition:
			background 0.2s ease-in-out,
			filter 0.2s ease-in-out;
		clip-path: polygon(var(--slant-px) 0, 100% 0, calc(100% - var(--slant-px)) 100%, 0 100%);
	}

	li.active a:link {
		background: var(--accent-color);
		background-size: 400% 200%;
		animation: background-scroll 5s linear infinite;
	}

	a:hover {
		cursor: pointer;
		filter: brightness(75%);
	}

	a:focus {
		background: var(--accent-color);
		background-size: 400% 200%;
		animation: background-scroll 5s linear infinite;
	}
</style>
