<script lang="ts">
	interface Props {
		onClick?: () => void;
		type?: 'button' | 'reset' | 'submit';
		isDisabled?: boolean;
		class?: string;
		children?: import('svelte').Snippet;
	}

	let {
		onClick = () => {},
		type = 'button',
		isDisabled = false,
		class: className = '',
		children,
	}: Props = $props();
</script>

<button class={className} {type} onclick={onClick} disabled={isDisabled}>
	{@render children?.()}
</button>

<style>
	button {
		--slant-px: 1rem;
		padding: 0.5em 1.5em;
		border: none;
		background: lightgray;
		transition: background 0.2s ease-in-out;
		clip-path: polygon(var(--slant-px) 0, 100% 0, calc(100% - var(--slant-px)) 100%, 0 100%);
	}

	button:hover {
		cursor: pointer;
		background: rgb(183, 183, 183);
	}

	button:focus {
		background: rgb(154, 154, 154);
	}

	button:disabled:hover {
		background: lightgray;
	}
</style>
