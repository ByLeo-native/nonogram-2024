import './index.css'; // Estilos CSS

function SolutionButton({ solutionMode, setSolutionMode, helpsActive }) {
    return (
        <button
            className={`click-button ${helpsActive ? 'unlock-button' : 'lock-button'} ${helpsActive ? 'solution-active' : ''}`}
            onClick={() => {
                if (helpsActive) {
                    setSolutionMode(!solutionMode);
                }
            }}
        >
            {solutionMode ? 'Desactivar Solución' : 'Mostrar Solución'}
            <span className={`status-indicator ${solutionMode ? 'active' : ''}`}></span>
        </button>
    );
}

export default SolutionButton;