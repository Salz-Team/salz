<script lang="ts">
	import type { MatchResult as MatchResultModel } from '$lib/models';
	import Icon from '@iconify/svelte';
	import CheckeredPattern from './CheckeredPattern.svelte';
	import RoundInitial from './RoundInitial.svelte';
	import Robot from './icons/Robot.svelte';

	export let matchResult: MatchResultModel;
	export let datetimeDisplay: string;
</script>

<article class="match-result">
	<time datetime={datetimeDisplay}>
		{datetimeDisplay}
	</time>

	<div class="match-result-summary">
		<figure class="bot-wrapper">
			{#if matchResult.winner === 'bot1'}
				<CheckeredPattern color="colorful" />
			{/if}
			<Robot hasCrown={matchResult.winner === 'bot1'} />
			<RoundInitial name={matchResult.bot1.owner?.username} />
		</figure>
		vs
		<figure class="bot-wrapper">
			{#if matchResult.winner === 'bot2'}
				<CheckeredPattern color="colorful" />
			{/if}
			<Robot hasCrown={matchResult.winner === 'bot2'} />
			<RoundInitial name={matchResult.bot2.owner?.username} />
		</figure>
	</div>
	<a href={`/matches?id=${matchResult.id}`}><Icon icon="tabler:video" /></a>
</article>

<style>
	.match-result {
		display: grid;
		grid-template-columns: 1fr 2fr 1fr;
		column-gap: 1.5em;
		align-items: center;
		position: relative;
	}

	.match-result *:nth-child(3) {
		justify-self: center;
		font-size: 3rem;
	}

	.match-result + :global(.match-result) {
		margin-top: 1rem;
	}
	.match-result + :global(.match-result::before) {
		content: '';
		width: 70%;
		height: 1px;
		background: linear-gradient(
			0.25turn,
			transparent,
			15%,
			rgba(100, 100, 100, 0.5),
			85%,
			transparent
		);
		position: absolute;
		top: -0.5rem;
		left: 50%;
		transform: translateX(-50%);
	}

	.match-result-summary {
		display: flex;
		justify-content: space-around;
		align-items: center;
	}
	.bot-wrapper {
		position: relative;
		width: 120px;
		height: 120px;
		display: inline-flex;
		justify-content: center;
		align-items: center;
	}
	.bot-wrapper :global(.checkered-pattern.custom .main) {
		filter: brightness(20%);
	}
	.bot-wrapper :global(.checkered-pattern) {
		opacity: 1;
		position: absolute;
		top: 0;
		left: 0;
		width: inherit;
	}
	.bot-wrapper :global(.robot) {
		position: relative;
		width: 100px;
		z-index: 1;
	}
	.bot-wrapper :global(.person-initials) {
		position: absolute;
		z-index: 2;
		top: 65%;
		right: 10px;
	}
</style>
