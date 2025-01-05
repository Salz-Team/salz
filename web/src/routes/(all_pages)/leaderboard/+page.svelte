<script lang="ts">
	import FancyHeader from '$lib/components/FancyHeader.svelte';
	import Paginator from '$lib/components/Paginator.svelte';
	import RoundInitial from '$lib/components/RoundInitial.svelte';
	import type { Player } from '$lib/models/player.model';

	let peopleScores: { person: Player; score: number }[] = $derived([
		{ person: { id: '1', username: 'h4ck0r1' }, score: 4320 },
		{ person: { id: '2', username: 'h4ck0r2' }, score: 4120 },
		{ person: { id: '3', username: 'h4ck0r3' }, score: 3710 },
	]);

	let totalPages = $derived(3);
	let currentPage = $derived(1);
	const maxPagesShown = 1;
</script>

<main>
	<FancyHeader>Leaderboard</FancyHeader>

	<section>
		{#each peopleScores as personScore, i}
			<article>
				{i + 1}
				<RoundInitial name={personScore.person.username} />
				<span>{personScore.person.username}</span>
				<span>{personScore.score}</span>
			</article>
		{/each}
	</section>

	<Paginator
		class="leaderboard-paginator"
		{totalPages}
		{currentPage}
		{maxPagesShown}
		noEdgeJumpers={true}
	/>
</main>

<style>
	main {
		width: 360px;
		margin: 0 auto;
		border: 1px solid black;
		padding: 6em 1em 2em;
		position: relative;
		border-radius: 5px;
	}

	main :global(.fancy-header) {
		position: absolute;
		top: 0.7em;
		left: 0;
	}

	article {
		display: flex;
		justify-content: space-between;
		align-items: center;
	}

	main :global(.leaderboard-paginator) {
		justify-content: space-between;
		width: 100%;
		margin-top: 1em;
	}
</style>
