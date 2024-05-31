import './index.css'; // Estilos CSS

function SolutionButton({ solutionMode, setSolutionMode }) {
    return (
        <button className={`click-button ${solutionMode ? 'solution-active' : ''}`} onClick={() => setSolutionMode(!solutionMode)}>
            {solutionMode ? 'Desactivar Revelar Celda' : 'Revelar Celda'}
        </button>
    );
}

export default SolutionButton;