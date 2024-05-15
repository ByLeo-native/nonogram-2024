import './index.css';

function WinnerBox({ winner}) {
  if (winner === null) return null

  const winnerText = winner ? '¡Has ganado!' : '';

  return (
    <section className='winner-overlay'>
      <div className='winner-box'>
        <h2 className='winner-text'>{winnerText}</h2>
        {winner && (
          <div className='winner-icon'>
            <img src='winner-icon.png' alt='Winner Icon' />
          </div>
        )}
      </div>
    </section>
  )
}

export default WinnerBox;