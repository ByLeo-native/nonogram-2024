import './index.css'; // Estilos CSS

function SolutionButton({ solutionMode, setSolutionMode, onClick }) {
    return (
        <button className={`click-button ${solutionMode ? 'solution-active' : ''}`} onClick={() => {
            setSolutionMode(!solutionMode);
            onClick();}}>
            {solutionMode ? 'Desactivar Solución' : 'Mostrar Solución'}
            <span className={`status-indicator ${solutionMode ? 'active' : ''}`}></span>
        </button>
    );
}

export default SolutionButton;