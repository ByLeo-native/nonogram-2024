import './index.css'; // Estilos CSS

function RevealButton({ revealMode, setRevealMode, helpsActive }) {
    return (
        <button
            className={`click-button ${helpsActive ? 'unlock-button' : 'lock-button'} ${helpsActive ? 'reveal-active' : ''}`}
            onClick={() => {
                if (helpsActive) {
                    setRevealMode(!revealMode);
                }
            }}
        >
            {revealMode ? 'Desactivar Revelar Celda' : 'Revelar Celda'}
            <span className={`status-indicator ${revealMode ? 'active' : ''}`}></span>
        </button>
    );
}

export default RevealButton;