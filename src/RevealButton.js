import './index.css'; // Estilos CSS

function RevealButton({ revealMode, setRevealMode }) {
    return (
        <button className={`click-button ${revealMode ? 'reveal-active' : ''}`} onClick={() => setRevealMode(!revealMode)}>
            {revealMode ? 'Desactivar Revelar Celda' : 'Revelar Celda'}
            <span className={`status-indicator ${revealMode ? 'active' : ''}`}></span>
        </button>
    );
}

export default RevealButton;