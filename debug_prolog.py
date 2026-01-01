
from src.search.manager import SessionManager

m = SessionManager()
print("Disorders:", m.state.active_candidates)

gad_syms = m.get_candidate_symptoms('gad')
print("\nGAD Symptoms Raw:")
print(gad_syms)

print("\nType check:")
if gad_syms:
    print(f"Type of first element: {type(gad_syms[0])}")
