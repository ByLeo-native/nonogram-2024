import './index.css'; // Estilos CSS para el toggle switch

function ToggleSwitch({paintedMode, setPaintedMode}) {

  const toggleSwitch = () => {
    setPaintedMode(!paintedMode);
  };

  return (
    <div className="toggle-switch-container" onClick={toggleSwitch}>
      <div className={`toggle-switch ${paintedMode ? 'checked' : ''}`}>
        {!paintedMode ? (
            <span className="toggle-switch-cross">X</span>
          ) : (
            <span className="toggle-switch-square"></span>
          )}
      </div>
    </div>
  );
}

export default ToggleSwitch;
